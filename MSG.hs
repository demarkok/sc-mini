module MSG (abstract) where

import Data
import DataUtil
import Data.List
import Data.Maybe

import Control.Monad.State

abstract :: NameSupply -> Expr -> Expr -> Gen
abstract ns e1 e2 = evalState (generalize e1 e2) (ns)

fresh :: State NameSupply Name
fresh = do -- could be done explicitly?
    (x : ns) <- get
    put ns
    return x

destructApp :: Expr -> [Expr]
destructApp (e1 :@: e2) = destructApp e1 ++ destructApp e2
destructApp e = [e]

generalize :: Expr -> Expr -> State NameSupply Gen
generalize (Ctr c1 args1) (Ctr c2 args2)
    | c1 == c2 && length args1 == length args2 = do 
        (args, th1, th2) <- generalizeList args1 args2
        return ((Ctr c1 args), th1, th2)
generalize (Call f1 args1) (Call f2 args2)
    | f1 == f2 && length args1 == length args2 = do
        (args, th1, th2) <- generalizeList args1 args2
        return ((Call f1 args), th1, th2)

generalize e1 e2
    | (Var x1 : tl1) <- destructApp e1
    , (Var x2 : tl2) <- destructApp e2
    , x1 == x2 && length tl1 == length tl2 -- TODO: what if x1 != x2?
    = do
        (args, th1, th2) <- generalizeList tl1 tl2
        let res = foldl (:@:) (Var x1) args
        return (res, th1, th2)

    | ((Lmb x1 body1) : tl1) <- destructApp e1
    , ((Lmb x2 body2) : tl2) <- destructApp e2
    , length tl1 == length tl2
    = do
        x <- fresh
        ((body : args), th1, th2) <- generalizeList (body1 : tl1) ((body2 // [(x2, Var x)]) : tl2)
        let res = foldl (:@:) (Lmb x body) args
        return (res, th1, th2)
    
    | ((Case e1 options1) : tl1) <- destructApp e1
    , ((Case e2 options2) : tl2) <- destructApp e2
    , length tl1 == length tl2
    , length options1 == length options2
    , Just patternSubsts <- sequenceA $ zipWith patternSubst (fst <$> options2) (fst <$> options1)
    = do
        ((e : applicands), phi1, phi2) <- generalizeList (e1 : tl1) (e2 : tl2)
        let optionBodies1 = map snd options1
        let optionBodies2 = zipWith (//) (map snd options2) patternSubsts
        (optionBodies, th1, th2) <- generalizeList optionBodies1 optionBodies2
        let options = zip (map fst options1) optionBodies
        let res = foldl (:@:) (Case e options) applicands
        return (res, phi1 ++ th1, phi2 ++ th2)
    | otherwise = do
        x <- fresh
        return (Var x, [(x, e1)], [(x, e2)])

generalizeList :: [Expr] -> [Expr] -> State NameSupply ([Expr], Subst, Subst)
generalizeList args1 args2 = do
    (args, ths1, ths2) <- unzip3 <$> zipWithM generalize args1 args2
    return (args, concat ths1, concat ths2)
