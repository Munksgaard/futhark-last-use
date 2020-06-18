{-# LANGUAGE NamedFieldPuns #-}

module LastUse (lastUseAction) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Map (Map)
import Futhark.IR.KernelsMem (KernelsMem, MemOp (..), Prog (..), freeIn, typeOf)
import Futhark.IR.Prop.Names (FreeDec, FreeIn, Names, boundByStm, boundInBody, namesFromList, namesSubtract, namesToList)
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Pipeline

lastUse :: Stms KernelsMem -> Map VName Int
lastUse stms =
  zip (toList stms) [0 ..]
    & reverse
    & foldr helper Map.empty
  where
    helper :: FreeIn a => (a, Int) -> Map VName Int -> Map VName Int
    helper (stm, i) m =
      freeIn stm
        & namesToList
        & foldr (flip Map.insert i) m

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

    zip (toList bodyStms) [0 ..]
      & fmap (\(stm, i) -> show i ++ ": " ++ pretty stm)
      & unlines
      & putStrLn
      & liftIO

    lastUse bodyStms
      & Map.toList
      & fmap (\(k, v) -> pretty k ++ ": " ++ show v)
      & unlines
      & putStrLn
      & liftIO

lastUseProg :: Prog KernelsMem -> FutharkM ()
lastUseProg (Prog _ funs) = mapM_ lastUseFun funs

lastUseAction :: Action KernelsMem
lastUseAction =
  Action
    { actionName = "memory allocation lastUse analysis",
      actionDescription = "Perform lastUse analysis on memory allocations",
      actionProcedure = lastUseProg
    }
