module Generator(residuate, isBase) where

import Data
import DataUtil


residuate :: Graph Conf -> Task
residuate tree = (expr, program) where
  (expr, program, _) = res nameSupply [] tree


--- generation of residual program
res :: NameSupply -> [(Conf, Conf)] -> Graph Conf -> (Conf, Program, NameSupply)
res ns mp (Node e Stop) = (e, Program [], ns)

res ns mp (Node (Ctr cname _) (Decompose ts)) = (Ctr cname args, p1, ns1) where
  (args, p1, ns1) = res' ns mp ts

res ns mp (Node (Let (v, _) _) (Decompose ts)) = (Let (v, e1) e2, p1, ns1) where
  ([e1, e2], p1, ns1) = res' ns mp ts

-- res ns mp (Node (Let (v, _) _) (Decompose ts)) = (e2 // [(v, e1)], p1, ns1) where
--   ([e1, e2], p1, ns1) = res' ns mp ts

res ns mp (Node (e1 :@: e2) (Decompose (var : args))) = (foldl (:@:) x args', p1, ns1) where
  Node x@(Var _) _ = var
  (args', p1, ns1) = res' ns mp args

res ns mp (Node (Lmb x e) (Transient t)) = (Lmb x e1, p1, ns1) where
  (e1, p1, ns1) = res ns mp t

-- TODO: add asserts
res ns mp (Node e (Variants contracts)) = (Case (Var e1) (zip pats exs), p1, ns1) where
  (Contract e1 _, _) = head $ contracts
  options = [(pat, ex) | (Contract x pat, ex) <- contracts]
  (exs, p1, ns1) = res' ns mp (map snd options)
  pats = map fst options

res ns mp (Node e (Fold (Node base _) ren)) = (call, Program [], ns) where
  call = baseCall // [(x, Var y) | (x, y) <- ren]
  Just baseCall = lookup base mp

res (n:ns) mp (Node e@(Call _ _) (Transient t)) = (fcall, Program ((FDef f1 vs body):fs), ns1) where
  vs = vnames e
  f1 = "ff" ++ (tail n)
  fcall = Call f1 $ map Var vs
  (body, Program fs, ns1) = res ns ((e, fcall) : mp) t

res ns mp (Node e (Transient t))
  | True = res ns mp t



-- proceeds a list of trees
-- the main goal is to handle name supply
res' :: NameSupply -> [(Conf, Conf)] -> [Graph Conf] -> ([Conf], Program, NameSupply)
res' ns mp ts = foldl f ([], Program [], ns) ts where
  f (cs, Program fs, ns1) t = (cs ++ [g], Program (fs ++ fs1), ns2) where
    (g, Program fs1, ns2) = res ns1 mp t

isBase e1 (Node _ (Decompose ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (Variants cs)) = or $ map (isBase e1 . snd) cs
isBase e1 (Node _ (Transient t)) = isBase e1 t
isBase e1 (Node _ (Fold (Node e2 _) _)) = e1 == e2
isBase e1 (Node e2 Stop) = False