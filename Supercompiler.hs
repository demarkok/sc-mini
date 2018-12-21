module Supercompiler where

import Data
import DataUtil
import Driving
import Folding
import Generator
import Data.List
import Embedding
import MSG


buildTree :: Task -> Graph Conf
buildTree (e, p) = simplify $ foldTree $ buildGTree (driveMachine p) e

buildTreeEmbedWhistle :: Task -> Graph Conf
buildTreeEmbedWhistle (e, p) = simplify $ foldTree $ buildGTree (driveMachine p) e


supercompile :: Task -> Task
supercompile = residuate . buildTree

sipercompileEmbedWhistle :: Task -> Task
sipercompileEmbedWhistle = residuate . buildTreeEmbedWhistle


buildFTree :: Machine Conf -> Conf -> Tree Conf
buildFTree m e = bft (addPropagation m) nameSupply e


buildGTree :: Machine Conf -> Conf -> Tree Conf
buildGTree m e = generalizeT m (('y':) <$> nameSupply) (bft (addPropagation m) nameSupply e)


bft :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bft d (n:ns) e | whistle e = bft d ns $ generalize n e
bft d ns     t | otherwise = case d ns t of
  Decompose ds -> Node t $ Decompose $ map (bft d ns) ds
  Transient e -> Node t $ Transient $ bft d ns e
  Stop -> Node t Stop
  Variants cs -> Node t $ Variants [(c, bft d (unused c ns) e) | (c, e) <- cs]


sizeBound = 100
whistle :: Expr -> Bool
whistle e@(Call _ args) = not (all isVar args) && size e > sizeBound
whistle _ = False


generalize :: Name -> Expr -> Expr
generalize n (Call f es) =
  Let (n, e) (Call f es') where (e, es') = extractArg n es

extractArg :: Name -> [Expr] -> (Expr, [Expr])
extractArg n es = (maxE, vs ++ Var n : ws) where
  maxE = maximumBy ecompare es
  ecompare x y = compare (eType x * size x) (eType y * size y)
  (vs, w : ws) = break (maxE ==) es
  eType e = if isVar e then 0 else 1



generalizeT :: Machine Conf -> NameSupply -> Node Conf -> Node Conf
generalizeT = generalizeT' []

generalizeT' :: [Node Conf] -> Machine Conf -> NameSupply -> Node Conf -> Node Conf
generalizeT' _ _ _ x@(Node t Stop) = x
generalizeT' h d ns (Node t (Decompose ds)) = Node t $ Decompose $ (map (generalizeT' h d ns) ds)
generalizeT' h d ns (Node t (Variants cs)) = Node t $ Variants [(c, generalizeT' h d (unused c ns) e) | (c, e) <- cs]
generalizeT' h d ns w@(Node t (Transient e))
  | _:_ <- [(k, r) | k <- h, isCall t, Just r <- [renaming (nodeLabel k) t]] 
  = w
  | k:_ <- [k | k <- h, isCall t, (nodeLabel k) <: t, isGood (nodeLabel k) t]
  , t' <- abstract ns t (nodeLabel k)
  = generalizeT' h d ns (bft d ns t')
  | otherwise
  = Node t $ Transient $ generalizeT' (h ++ [w]) d ns e


addPropagation :: Machine Conf -> Machine Conf
addPropagation m ns e = propagateContract (m ns e)

propagateContract :: Step Conf -> Step Conf
propagateContract (Variants vs) =
  Variants [(c, e // [(v, Ctr cn $ map Var vs)]) |
            (c@(Contract v (Pat cn vs)), e) <- vs]
propagateContract step = step


simplify :: Graph Conf -> Graph Conf
simplify (Node e (Decompose ts)) =
  Node e (Decompose $ map simplify ts)
simplify (Node e (Variants cs)) =
  Node e (Variants [(c, simplify t) | (c, t) <- cs])
simplify (Node e@(Call _ _) (Transient t)) | isBase e t =
  Node e $ Transient $ simplify t
simplify (Node e (Transient t)) =
  simplify t
simplify t = t