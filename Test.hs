module Test where

import Data
import Driving
import DataUtil
import DataIO
import Folding
import Generator
import Supercompiler
import MSG
import Embedding
import Data.List

putTree = putStr . (printTree 100)

kmp :: Program
kmp = read
  " fEqSymb(x, y) = case x of [A() => fEqA(y) | B() => fEqB(y)];\
  \ fEqA(x) = case x of [A() => True() | B() => False()];\
  \ fEqB(x) = case x of [B() => True() | A() => False()];\
  \ fIf(cond, x, y) = case cond of [True() => x | False() => y];\
  \ fMatch(pat, str) = fM(pat, str, pat, str);\
  \ fM(pat, str, savePat, saveStr) = case pat of [Nil() => True() |\ 
  \                                               Cons(p, pp) => fX(str, p, pp, savePat, saveStr)];\
  \ fX(str, p, pp, savePat, saveStr) = case str of [Nil() => False() |\
  \                                                 Cons(s, ss) => fIf(fEqSymb(p, s), fM(pp, ss, savePat, saveStr), fN(saveStr, savePat))];\
  \ fN(y, savePat) = case y of [Nil() => False() | Cons(s, ss) => fM(savePat, ss, savePat, ss)];"

kmpExpr1 :: Expr
kmpExpr1 = read "fMatch( Cons(A(), Cons(B(), Nil())), q)"

kmpPrintTree1 :: IO ()
kmpPrintTree1 = putTree $ buildTree (kmpExpr1, kmp)

kmpResult1 :: Task
kmpResult1 = supercompile (kmpExpr1, kmp)

kmpExpr2 :: Expr
kmpExpr2 = read "fMatch(Cons(A(), Cons(B(), Cons(A(), Cons(A(), Nil())))), q)"


kmpPrintTree2 :: IO ()
kmpPrintTree2 = putTree $ buildTree (kmpExpr2, kmp)

kmpResult2 :: Task
kmpResult2 = supercompile (kmpExpr2, kmp)
