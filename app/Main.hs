module Main where

import GHC.Paths (libdir)
import DynFlags (defaultFatalMessager, defaultFlushOut)
import Control.Monad (when)
import System.FilePath
import System.Posix.Files (fileExist, removeLink)
import Options.Applicative
import Outputable (ppr, showSDocUnsafe)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Rules (rulesOfBinds)


import Lib (compileToCoreModule, CoreModule(..), defaultErrorHandler, getSessionDynFlags, setSessionDynFlags, runGhc)


newtype CmdArgs = CmdArgs
    { filename :: FilePath }


cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
    <$> argument str
        ( metavar "FILENAME"
        <> help "Path to Haskell program file")


main :: IO ()
main = compile =<< execParser opts
    where 
        opts = info (cmdArgs <**> helper)
          ( fullDesc
            <> progDesc "Output all rewrite rules in FILENAME to stdout"
            <> header 
            "ghc-rules -- Visualize all GHC Core rules defined in a single Haskell program"
         )



compile :: CmdArgs -> IO ()
compile (CmdArgs inputfile) = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    inputfileExists <- fileExist inputfile
    
    cleanArtifacts inputfile

    if not inputfileExists 
       then do
           putStrLn $ "File not found :" ++ show inputfile
           return ()
       else
           runGhc (Just libdir) $ do
               dflags <- getSessionDynFlags
               setSessionDynFlags dflags
               coreM <- compileToCoreModule inputfile
               let binders = cm_binds coreM
                   rules_of_bndrs = rulesOfBinds binders
                   rules_of_guts  = cm_rules coreM

               liftIO $ putStrLn $ showSDocUnsafe(ppr (rules_of_bndrs ++ rules_of_guts))
               return ()

    cleanArtifacts inputfile


cleanArtifacts inputfile =
    let oFile = replaceExtension inputfile ".o"
        hiFile = replaceExtension inputfile ".hi"
     in do
         has_ofile <- fileExist oFile
         has_hifile <- fileExist hiFile

         when has_ofile (removeLink oFile)
         when has_hifile (removeLink hiFile)
