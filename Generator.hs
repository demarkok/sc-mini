module Generator where
	
import Language
import Driving
import Data.Maybe
import Data.List

-- simplifies tree - removes transient edges
s :: Tree -> Tree
s (Node e (DecomposeStep ts)) = 
	(Node e (DecomposeStep $ map s ts))

s (Node e (ContractStep cs)) = 
	Node e $ ContractStep $ map (\(c, t) -> (c, s t)) cs

s (Node e (TransientStep t@(Node e1 step))) | isBase e t = 
	if isBase e1 t then Node e $ TransientStep $ s t else Node e step1 where
		Node _ step1 = s t

s (Node e (TransientStep t)) = 
	s t

s t = t

isBase e1 (Node _ (DecomposeStep ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (ContractStep cs)) = or $ map (isBase e1 . snd) cs 
isBase e1 (Node _ (TransientStep t)) = isBase e1 t
isBase e1 (Node e2 Fold) = isJust $ renaming e2 e1
isBase e1 (Node e2 Stop) = False

--- generation of residual program
res :: NameSupply -> [(Term, Term)] -> Tree -> (Term, Program, NameSupply)
res ns mp (Node e Stop) = (e, Program [] [], ns)

res ns mp (Node (Ctr cname _) (DecomposeStep ts)) = (Ctr cname args, p1, ns1) where
	(args, p1, ns1) = make ns mp ts

res ns mp (Node (Let v _ _) (DecomposeStep ts)) = (Let v e1 e2, p1, ns1) where
	([e1, e2], p1, ns1) = make ns mp ts

res (n:ns) mp (Node e (TransientStep t)) = (fcall, Program ((FFun f1 vs body):fs) gs, ns1) where
	vs = vnames e
	f1 = "f" ++ n
	fcall = FCall f1 $ map Var vs
	(body, Program fs gs, ns1) = res ns ((e, fcall) : mp) t
	
res (n:ns) mp (Node e (ContractStep cs)) = (gcall, Program fs (newGs ++ gs), ns1) where
	vs@(pv:vs') = vnames e
	g1 = "g" ++ n
	gcall = GCall g1 $ map Var vs
	(bodies, Program fs gs, ns1) = make ns ((e, gcall) : mp) $ map snd cs
	pats = map (\(Contract v pat, _) -> pat) cs
	newGs = map (\(p, b) -> GFun g1 p vs' b) (zip pats bodies)
	
res ns mp (Node e Fold) = (call1, Program [] [], ns) where
	call1 = subst ren1 call where
		(ren, call) = fromJust $ fromJust $ find isJust $ map (\(was, new) -> fmap (\ren -> (ren, new)) (renaming was e)) mp
		ren1 = map (\(x, y) -> (x, Var y)) ren

make :: NameSupply -> [(Term, Term)] -> [Tree] -> ([Term], Program, NameSupply)
make ns mp ts = foldl f ([], Program [] [], ns) ts where 
	f (gens, Program fs gs, ns1) tree = (gens ++ [g], Program (fs ++ fs1) (gs ++ gs1), ns2) where 
		(g, Program fs1 gs1, ns2) = res ns1 mp tree
		
vnames :: Term -> [String]
vnames (Var v) = [v]
vnames (Ctr _ args)   = nub $ concat $ map vnames args
vnames (FCall _ args) = nub $ concat $ map vnames args
vnames (GCall _ args) = nub $ concat $ map vnames args