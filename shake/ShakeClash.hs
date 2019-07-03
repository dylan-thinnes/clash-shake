{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module ShakeClash
    ( ClashProject(..)
    , ClashKit(..), buildDir
    , mainFor, mainForCustom
    ) where

import Development.Shake hiding ((~>))
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Control.Monad.Trans

import Text.Mustache
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.List (sort, nub)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Control.Monad (guard, msum)

import Clash.Driver.Types

data HDL
    = VHDL
    | Verilog
    | SystemVerilog
    deriving (Eq, Enum, Bounded, Show, Read)

hdlDir :: HDL -> FilePath
hdlDir VHDL = "vhdl"
hdlDir Verilog = "verilog"
hdlDir SystemVerilog = "systemverilog"

hdlFromDir :: FilePath -> Maybe HDL
hdlFromDir "vhdl" = Just VHDL
hdlFromDir "verilog" = Just Verilog
hdlFromDir "systemverilog" = Just SystemVerilog
hdlFromDir _ = Nothing

hdlExt :: HDL -> FilePath
hdlExt VHDL = "vhdl"
hdlExt Verilog = "v"
hdlExt SystemVerilog = "sv"

data XilinxTarget = XilinxTarget
    { targetFamily :: String
    , targetDevice :: String
    , targetPackage :: String
    , targetSpeed :: String
    }

instance ToMustache XilinxTarget where
    toMustache = object . targetMustache

targetMustache XilinxTarget{..} =
    [ "targetFamily" ~> T.pack targetFamily
    , "targetDevice" ~> T.pack targetDevice
    , "targetPackage" ~> T.pack targetPackage
    , "targetSpeed" ~> T.pack targetSpeed
    ]

boards :: M.Map String XilinxTarget
boards = M.fromList
    [ ("papilio-pro", XilinxTarget "Spartan6" "xc6slx9" "tqg144" "-2")
    , ("papilio-one", XilinxTarget "Spartan3E" "xc3s500e" "vq100" "-5")
    ]

data ClashProject = ClashProject
    { projectName :: String
    , clashModule :: String
    , clashTopName :: String
    , topName :: String
    , clashFlags :: [String]
    , shakeDir :: String
    , extraGenerated :: ClashKit -> [FilePath]
    }

data ClashKit = ClashKit
    { clash :: String -> [String] -> Action ()
    , xilinx :: String -> [String] -> Action ()
    }

buildDir :: FilePath
buildDir = "_build"

getBoard :: Action String
getBoard = fromMaybe "papilio-pro" <$> getConfig "BOARD"

getBoardConfig :: Action (Maybe String)
getBoardConfig = getConfig "BOARDCONFIG"

getFilesForBoard :: FilePath -> [FilePattern] -> Action [FilePath]
getFilesForBoard dir pats = do
    board <- getBoard
    cfg <- getBoardConfig
    files <- fmap mconcat . sequenceA $ concat $
      [ [ map (dropDirectoryN 1) <$> getDirectoryFiles "" (map (dir </>)                     pats)
        , map (dropDirectoryN 2) <$> getDirectoryFiles "" (map ((dir </> board) </>)         pats)
        ]
      , [ map (dropDirectoryN 3) <$> getDirectoryFiles "" (map ((dir </> board </> cfg) </>) pats)
        | Just cfg <- return cfg
        ]
      ]
    return $ nub . sort $ files
  where
    dropDirectoryN 0 = id
    dropDirectoryN n = dropDirectoryN (n - 1) . dropDirectory1

getFileForBoard :: FilePath -> FilePath -> Action FilePath
getFileForBoard dir file = do
    board <- getBoard
    cfg <- getBoardConfig
    overrideDir <- fmap msum . sequenceA . concat $
                   [ [ checkOverride (board </> cfg)
                     | Just cfg <- return cfg ]
                   , [ checkOverride board
                     ]
                   ]
    let dir' = maybe dir (dir </>) overrideDir
    return $ dir' </> file
  where
    checkOverride subdir = do
        exists <- doesFileExist (dir </> subdir </> file)
        return $ guard exists >> return subdir


mainFor :: ClashProject -> IO ()
mainFor proj = mainForCustom proj $ \_ -> pure ()

mainForCustom :: ClashProject -> (ClashKit -> Rules ()) -> IO ()
mainForCustom ClashProject{..} customRules = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile "build.mk"
    let mainHDL = Verilog

    let clash cmd args = do
            clashExe <- fromMaybe ("clash") <$> getConfig "CLASH"
            cmd_ clashExe ([cmd, "-i" <> "src-clash", "-outputdir", buildDir] <> clashFlags <> args)
        xilinx tool args = do
            root <- getConfig "XILINX_ROOT"
            wrap <- getConfig "XILINX"
            let exe = case (wrap, root) of
                    (Just wrap, _) -> [wrap, tool]
                    (Nothing, Just root) -> [root </> "ISE/bin/lin64" </> tool]
                    (Nothing, Nothing) -> error "XILINX_ROOT or XILINX must be set"
            cmd_ (Cwd buildDir) exe args

    let kit = ClashKit{..}

    let manifest hdl = buildDir </> hdlDir hdl </> clashModule </> clashTopName </> clashTopName <.> "manifest"

    let manifestSrcs hdl = do
            manifest <- pure $ manifest hdl
            need (manifest : extraGenerated kit)
            Manifest{..} <- read <$> readFile' manifest
            let clashSrcs = map T.unpack componentNames <>
                            [ map toLower clashTopName <> "_types" | hdl == VHDL ]
            hdlSrcs <- getFilesForBoard "src-hdl" ["*.vhdl", "*.v", "*.ucf"]
            coreSrcs <- getFilesForBoard "ipcore_dir" ["*"]
            return $ mconcat
              [ [ hdlDir hdl </> clashModule </> clashTopName </> c <.> hdlExt hdl | c <- clashSrcs ]
              , [ "src-hdl" </> hdl | hdl <- hdlSrcs ]
              , [ "ipcore_dir" </> core | core <- coreSrcs ]
              ]

        ipCores = do
            srcs <- getFilesForBoard "ipcore_dir" ["*.xco"]
            return [ dropExtension src | src <- srcs ]

    want [ buildDir </> topName <.> "bit" ]

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

    phony "clashi" $ do
        let src = "src-clash" </> clashModule <.> "hs" -- TODO
        clash "clashi" [src]

    phony "clash-vhdl" $ do
        need [manifest VHDL]

    phony "clash-verilog" $ do
        need [manifest Verilog]

    phony "clash-systemverilog" $ do
        need [manifest SystemVerilog]

    phony "ise" $ do
        -- need [buildDir </> projectName <.> "tcl"]
        xilinx "ise" [buildDir </> projectName <.> "tcl"]

    customRules kit

    buildDir <//> "*.manifest" %> \out -> do
        let src = "src-clash" </> clashModule <.> "hs" -- TODO
            hdlDir = takeDirectory1 . dropDirectory1 $ out
        hdl <- maybe (fail $ unwords ["Unknown HDL:", hdlDir]) return $ hdlFromDir hdlDir
        alwaysRerun
        need [ src ]
        clash "clash" [case hdl of { VHDL -> "--vhdl"; Verilog -> "--verilog"; SystemVerilog -> "--systemverilog" }, src]

    buildDir </> topName <.> "bit" %> \_out -> do
        srcs <- manifestSrcs mainHDL
        need $ mconcat
          [ [ buildDir </> projectName <.> "tcl" ]
          , [ buildDir </> src | src <- srcs ]
          ]
        xilinx "xtclsh" [projectName <.> "tcl", "rebuild_project"]

    buildDir <//> "*.tcl" %> \out -> do
        let src = shakeDir </> "project.tcl.mustache"
        s <- T.pack <$> readFile' src
        alwaysRerun

        board <- getBoard
        let target = fromMaybe (error $ unwords ["Unknown target board:", board]) $ M.lookup board boards

        srcs <- manifestSrcs mainHDL
        cores <- ipCores

        template <- case compileTemplate src s of
            Left err -> fail (show err)
            Right template -> return template
        let values = object . mconcat $
                     [ [ "project" ~> T.pack projectName ]
                     , [ "top" ~> T.pack topName ]
                     , targetMustache target
                     , [ "srcs" ~> [ object [ "fileName" ~> src ] | src <- srcs ] ]
                     , [ "ipcores" ~> [ object [ "name" ~> core ] | core <- cores ] ]
                     ]
        writeFileChanged out . T.unpack $ substitute template values

    buildDir </> "src-hdl" <//> "*" %> \out -> do
        src <- getFileForBoard "src-hdl" (dropDirectory1 . dropDirectory1 $ out)
        copyFileChanged src out

    buildDir </> "ipcore_dir" <//> "*" %> \out -> do
        src <- getFileForBoard "ipcore_dir" (dropDirectory1 . dropDirectory1 $ out)
        copyFileChanged src out
