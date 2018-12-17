module DataUtil(
  isValue,isCall,isVar,size,
  fDef, chooseOption,
  (//), renaming, vnames,nameSupply,
  nodeLabel,isRepeated,unused,patternSubst
  ) where

import Data
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad


isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args
isValue (Lmb _ _) = True
isValue _ = False

isCall :: Expr -> Bool
isCall (Call _ _) = True
isCall _ = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

fDef :: Program -> Name -> FDef
fDef (Program fs) fname = head [f | f@(FDef x _ _) <- fs, x == fname]

-- TODO: FIX renaming
chooseOption :: Expr -> Expr
chooseOption (Case (Ctr c cargs) options) = head [e // zip cvars cargs | (Pat c' cvars, e) <- options, c' == c]

filterSub :: Subst -> [Name] -> Subst
filterSub subst blackList = filter (\(x, _) -> notElem x blackList) subst

(//) :: Expr -> Subst -> Expr
(Var x) // sub = maybe (Var x) id (lookup x sub)
(Ctr name args) // sub = Ctr name (map (// sub) args)
(Call name args) // sub = Call name (map (// sub) args)
(Let (x, e1) e2) // sub  = Let (x', (e1 // sub)) (e2' // (filterSub sub [x'])) where
  x' = substRedexNewName x e2 sub
  e2' = e2 // [(x, Var x')]

(Case e options) // sub = Case (e // sub) (map f options) where
  f (pat@(Pat c args), option) = ((Pat c args'), option' // (filterSub sub args)) where
    args' = map q args
    q x = substRedexNewName x option sub
    option' = option // zip args (map Var args')

(Lmb x e) // sub = Lmb x' (e' // (filterSub sub [x'])) where
  x' = substRedexNewName x e sub
  e' = e // [(x, Var x')]

(e1 :@: e2) // sub = (e1 // sub) :@: (e2 // sub)

substRedexNewName :: Name -> Expr -> Subst -> Name
substRedexNewName x xBound xSubst
  | any (elem x) $ (vnames . snd) <$> filter ((`elem` (vnames xBound)) . fst) xSubst = newName x
  | otherwise = x
  
newName x = (x ++ "'" ++ (show $ length x))

nameSupply :: NameSupply
nameSupply = ["v" ++ (show i) | i <- [1 ..] ]

unused :: Contract -> NameSupply -> NameSupply
unused (Contract _ (Pat _ vs)) = (\\ vs)


vnames :: Expr -> [Name]
vnames = nub . vnames'

vnames' :: Expr -> [Name]
vnames' (Var v) = [v]
vnames' (Ctr _ args)   = concat $ map vnames' args
vnames' (Call _ args) = concat $ map vnames' args
vnames' (Let (x, e1) e2) = vnames' e1 ++ (vnames' e2 \\ [x])
vnames' (Case e options) = vnames' e ++ (concat $ map f options) where
  f (Pat _  pt, option) = vnames' option \\ pt
vnames' (Lmb x e) = (vnames' e) \\ [x]
vnames' (e1 :@: e2) = vnames' e1 ++ vnames' e2

isRepeated :: Name -> Expr -> Bool
isRepeated vn e = (length $ filter (== vn) (vnames' e)) > 1


-- TODO: SIMPLIFY!!
-- returns renaming applying which to the first expression you get the second one. 
renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = f $ partition isNothing $ renaming' (e1, e2) where
  f (x:_, _) = Nothing
  f (_, ps) = g gs1 gs2
    where
      gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
      gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy j $ nub $ catMaybes ps
      h (a, b) (c, d) = compare a c
      j (a, b) (c, d) = compare b d
  g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys
    then Just (concat xs) else Nothing

renaming' :: (Expr, Expr) -> [Maybe (Name, Name)]
renaming' ((Var x), (Var y)) = [Just (x, y)]
renaming' ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' ((Call n1 args1), (Call n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' (Let (v, e1) e2, Let (v', e1') e2') = renaming' (e1, e1') ++ renaming' (e2, e2' // [(v, Var v')])
renaming' (Case e ops, Case e' ops') | (length ops == length ops') = renaming' (e, e') ++ (concat $ map f $ zip ops ops') where
  f ((Pat c1 vars1, option1), (Pat c2 vars2, option2))
    | c1 == c2 && (length vars1 == length vars2) = renaming' (option1, option2 // zip vars1 (map Var vars2))
renaming' ((a1 :@: b1), (a2 :@: b2)) = renaming' (a1, a2) ++ renaming' (b1, b2)
renaming' ((Lmb v e), (Lmb v' e')) = renaming' (e, e' // [(v, Var v')])
renaming' _  = [Nothing]

size :: Expr -> Integer
size (Var _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (Call _ args) = 1 + sum (map size args)
size (Let (_, e1) e2) = 1 + (size e1) + (size e2)
size (Case e ops) = 1 + (size e) + sum (map f ops) where
  f ((Pat _ vars), option) = 1 + size option
size (Lmb _ e) = 1 + size e
size (e1 :@: e2) = 1 + size e1 + size e2

nodeLabel :: Node a -> a
nodeLabel (Node l _) = l

step :: Node a -> Step (Graph a)
step (Node _ s) = s

patternSubst :: Pat -> Pat -> Maybe Subst
patternSubst (Pat c1 args1) (Pat c2 args2) = do
    guard $ c1 == c2
    renameList args1 args2

renameList :: [Name] -> [Name] -> Maybe Subst
renameList x y = do
    guard $ length x == length y
    let r = zip x y
    let gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub r where
        h (a, b) (c, d) = compare a c
    let gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy j $ nub r where
        j (a, b) (c, d) = compare b d
    guard $ all ((== 1) . length) gs1
    guard $ all ((== 1) . length) gs2
    return $ zip x (Var <$> y)

