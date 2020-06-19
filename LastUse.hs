{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module LastUse (lastUseAction) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Futhark.Analysis.Alias (analyseStms)
import Futhark.IR.Aliases (CanBeAliased)
import Futhark.IR.KernelsMem (freeIn)
import Futhark.IR.Prop (ASTLore)
import Futhark.IR.Prop.Names (FreeIn, Names, namesToList)
import Futhark.IR.Syntax
import Futhark.Pipeline

lastUse :: ASTLore lore => Map VName Names -> Stms lore -> Map VName Int
lastUse aliases stms =
  let withoutAliasing =
        zip (toList stms) [0 ..]
          & reverse
          & foldr helper Map.empty
   in Map.foldrWithKey applyAliases withoutAliasing withoutAliasing
  where
    helper :: FreeIn a => (a, Int) -> Map VName Int -> Map VName Int
    helper (stm, i) m =
      freeIn stm
        & namesToList
        & foldr (`Map.insert` i) m
    applyAliases :: VName -> Int -> Map VName Int -> Map VName Int
    applyAliases vname0 line_num m0 =
      foldr (\vname m -> Map.insertWith max vname line_num m) m0 (maybe [] namesToList $ aliases !? vname0)

lastUseFun :: (ASTLore lore, CanBeAliased (Op lore)) => FunDef lore -> FutharkM ()
lastUseFun
  FunDef
    { funDefName,
      funDefParams,
      funDefBody = Body {bodyDec, bodyStms, bodyResult}
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

    let (stms, (aliases, _)) = analyseStms Map.empty bodyStms

    zip (toList stms) [0 :: Int ..]
      & fmap (\(stm, i) -> show i ++ ": " ++ show stm)
      & unlines
      & putStrLn
      & liftIO

    pretty aliases
      & putStrLn
      & liftIO

    zip (toList bodyStms) [0 :: Int ..]
      & fmap (\(stm, i) -> show i ++ ": " ++ pretty stm)
      & unlines
      & putStrLn
      & liftIO

    lastUse aliases bodyStms
      & Map.toList
      & fmap (\(vname, line_num) -> pretty vname ++ ": " ++ show line_num)
      & unlines
      & putStrLn
      & liftIO

lastUseProg :: (ASTLore lore, CanBeAliased (Op lore)) => Prog lore -> FutharkM ()
lastUseProg (Prog _ funs) = mapM_ lastUseFun funs

lastUseAction :: (ASTLore lore, CanBeAliased (Op lore)) => Action lore
lastUseAction =
  Action
    { actionName = "memory allocation lastUse analysis",
      actionDescription = "Perform lastUse analysis on memory allocations",
      actionProcedure = lastUseProg
    }
