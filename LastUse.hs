{-# LANGUAGE NamedFieldPuns #-}

module LastUse (lastUseAction) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Set as Set
import Data.Set (Set)
import Futhark.IR.KernelsMem (KernelsMem, MemOp (..), Prog (..), freeIn, typeOf)
import Futhark.IR.Prop.Names (Names, boundByStm, boundInBody, namesFromList, namesSubtract)
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Pipeline

lastUseFun :: FunDef KernelsMem -> FutharkM ()
lastUseFun
  FunDef
    { funDefEntryPoint,
      funDefName,
      funDefParams,
      funDefBody = body@Body {bodyDec, bodyStms, bodyResult}
    } = do
    liftIO $ putStrLn $ "Analyzing " ++ show funDefName
    liftIO $
      putStrLn $
        unwords
          [ "Params:",
            show funDefParams,
            "\nBodyDec:",
            show bodyDec,
            "\nBodyResult:",
            show bodyResult
          ]
    liftIO $
      putStrLn $
        unlines $
          toList $
            fmap (\stm -> pretty stm) $
              bodyStms

lastUseProg :: Prog KernelsMem -> FutharkM ()
lastUseProg (Prog _ funs) = mapM_ lastUseFun funs

lastUseAction :: Action KernelsMem
lastUseAction =
  Action
    { actionName = "memory allocation lastUse analysis",
      actionDescription = "Perform lastUse analysis on memory allocations",
      actionProcedure = lastUseProg
    }
