module Lib
    ( compileToCoreModule, 
      CoreModule (..),
      module GHC
    ) where

import GHC hiding (compileToCoreModule, compileCore, compileToCoreSimplified, CoreModule(..))
import HscTypes (typeEnvFromEntities, msHsFilePath, TypeEnv, CgGuts(..), ModDetails(..), ModGuts(..))
import CoreSyn (CoreProgram, CoreRule, bindersOfBinds)
import Panic (panic)
import TidyPgm (tidyProgram)
import HscMain (hscSimplify)
import TcRnTypes (tcg_th_coreplugins, TcGblEnv)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Data.Foldable (find)
import Data.IORef (readIORef)


-- | A CoreModule consists of just the fields of a 'ModGuts' that are needed for
-- the 'GHC.compileToCoreModule' interface.
data CoreModule
  = CoreModule {
      -- | Module name
      cm_module   :: !Module,
      -- | Type environment for types declared in this module
      cm_types    :: !TypeEnv,
      -- | Declarations
      cm_binds    :: CoreProgram,
      -- | Safe Haskell mode
      cm_safe     :: SafeHaskellMode,

      cm_rules :: ![CoreRule]          -- ^ Before the core pipeline starts, contains
                                       -- See Note [Overall plumbing for rules] in Rules.hs
      
    }


compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compileToCoreModule = compileCore False

-- | Like compileToCoreModule, but invokes the simplifier, so
-- as to return simplified and tidied Core.
compileToCoreSimplified :: GhcMonad m => FilePath -> m CoreModule
compileToCoreSimplified = compileCore True

compileCore :: GhcMonad m => Bool -> FilePath -> m CoreModule
compileCore simplify fn = do
   -- First, set the target to the desired filename
   target <- guessTarget fn Nothing
   addTarget target
   _ <- load LoadAllTargets
   -- Then find dependencies
   modGraph <- depanal [] True
   case find ((== fn) . msHsFilePath) (mgModSummaries modGraph) of
     Just modSummary -> do
       -- Now we have the module name;
       -- parse, typecheck and desugar the module
       (tcg, mod_guts) <- -- TODO: space leaky: call hsc* directly?
         do tm <- typecheckModule =<< parseModule modSummary
            let tcg = fst (tm_internals_ tm)
            (,) tcg . coreModule <$> desugarModule tm
       liftM (gutsToCoreModule (mg_safe_haskell mod_guts)) $
         if simplify
          then do
             -- If simplify is true: simplify (hscSimplify), then tidy
             -- (tidyProgram).
             hsc_env <- getSession
             simpl_guts <- liftIO $ do
               plugins <- readIORef (tcg_th_coreplugins tcg)
               hscSimplify hsc_env plugins mod_guts
             tidy_guts <- liftIO $ tidyProgram hsc_env simpl_guts
             return $ Left tidy_guts
          else
             return $ Right mod_guts

     Nothing -> panic "compileToCoreModule: target FilePath not found in module dependency graph"
  where -- two versions, based on whether we simplify (thus run tidyProgram,
        -- which returns a (CgGuts, ModDetails) pair, or not (in which case
        -- we just have a ModGuts.
        gutsToCoreModule :: SafeHaskellMode
                         -> Either (CgGuts, ModDetails) ModGuts
                         -> CoreModule
        gutsToCoreModule safe_mode (Left (cg, md)) = CoreModule {
          cm_module = cg_module cg,
          cm_types  = md_types md,
          cm_binds  = cg_binds cg,
          cm_safe   = safe_mode,
          cm_rules  = [] -- Sorry Sempai, no rules left in my guts after simplification
        }
        gutsToCoreModule safe_mode (Right mg) = CoreModule {
          cm_module  = mg_module mg,
          cm_types   = typeEnvFromEntities (bindersOfBinds (mg_binds mg))
                                           (mg_tcs mg)
                                           (mg_fam_insts mg),
          cm_binds   = mg_binds mg,
          cm_safe    = safe_mode,
          cm_rules    = mg_rules mg
         }

