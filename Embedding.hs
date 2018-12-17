module Embedding where

import Data
import DataUtil
import Data.List
import Data.Maybe
import Control.Monad.State

infixl 3 <:
(<:) :: Expr -> Expr -> Bool

coupl :: Expr -> Expr -> Bool
divin :: Expr -> Expr -> Bool
embed :: Expr -> Expr -> Bool


e1 <: e2 = or $ do
    let fv1 = vnames e1
    let fv2 = vnames e2
    right <- replicateM (length fv1) fv2
    let renaming = zip fv1 (map Var right)
    return $ (e1 // renaming) `embed` e2

-- are (x y) z and x (y z) coupled?

e1 `embed` e2 = e1 `coupl` e2 || e1 `divin` e2

(Var x) `coupl` (Var y) = x == y
(Call f1 args1) `coupl` (Call f2 args2) = f1 == f2 && (and $ zipWith embed args1 args2)
(Ctr c1 args1) `coupl` (Ctr c2 args2) = c1 == c2 && (and $ zipWith embed args1 args2)
(e1 :@: e1') `coupl` (e2 :@: e2') = e1 `embed` e2 && e1' `embed` e2'
(Case e1 options1) `coupl` (Case e2 options2) = e1 `embed` e2 && (and $ zipWith f options1 options2) where 
        f (p1, option1) (p2, option2)
            | Just subst <- patternSubst p2 p1
            = option1 `embed` (option2 // subst)
            | otherwise = False
(Lmb x1 e1) `coupl` (Lmb x2 e2) = e1 `embed` (e2 // [(x2, Var x1)])
_ `coupl` _ = False

e `divin` (Ctr c args) = any (embed e) args
e `divin` (Call f args) = any (embed e) args
e `divin` (Lmb x e') = e `embed` e'
e `divin` (e1 :@: e2) = e `embed` e1 || e `embed` e2
e `divin` (Case _ options) = any (embed e . snd) options
_ `divin` _ = False
