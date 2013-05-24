module Deforester where

import Data
import DataUtil
import Driving
import Folding

--deforest :: Task -> Task
--deforest (e, p) =
--	residuate $ simplify $ foldTree $ buildTree (driveMachine p) e
	
simplify :: Graph Conf -> Graph Conf
simplify (Node e (EDecompose comp ts)) = 
	Node e (EDecompose comp $ map simplify ts)
simplify (Node e (EVariants cs)) = 
	Node e (EVariants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (ETransient tr t)) | isBase e t = 
	Node e $ ETransient tr $ simplify t 
simplify (Node e (ETransient _ t)) = 
	simplify t
simplify t = t

isBase e1 (Node _ (EDecompose _ ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (EVariants cs)) = or $ map (isBase e1 . snd) cs 
isBase e1 (Node _ (ETransient _ t)) = isBase e1 t
isBase e1 (Node _ (EFold (Node e2 _) _)) = e1 == e2
isBase e1 (Leaf e2) = False
