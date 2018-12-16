module Driving where

import Data
import DataUtil

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m nameSupply e

bt :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bt m ns c = case m ns c of
  Decompose ds -> Node c $ Decompose (map (bt m ns) ds)
  Transient e -> Node c $ Transient (bt m ns e)
  Stop -> Node c Stop
  Variants cs -> Node c $ Variants [(c, bt m (unused c ns) e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = drive where
  drive :: Machine Conf
  drive ns (Var _) = Stop
  drive ns (Ctr _ []) = Stop
  drive ns (Ctr _ args) = Decompose args
  drive ns (Let (x, t1) t2) = Decompose [t1, t2]
  drive ns (Call name args) = Transient $ e // (zip vs args) where
    FDef _ vs e = fDef p name
  
  drive ns (Lmb _ e) = Transient e
  
  drive ns (var@(Var _) :@: e2) = Decompose [var, e2]
  drive ns ((Lmb x e1) :@: e2) = Transient $ e1 // [(x, e2)]
  drive ns (e1 :@: e2) = inject (drive ns e1) where
    inject (Transient t) = Transient (t :@: e2)
    inject (Variants cs) = Variants $ map f cs 
    -- Then We're sure that e1 is an application cause `Let :@: e2' and `Ctr :@: e2' are impossible
    inject (Decompose apps) = Decompose $ apps ++ [e2] 
    f (c, t) = (c, t :@: e2)

  drive ns redex@(Case (Ctr c cargs) options) = Transient $ chooseOption redex
  drive ns (Case (Var x) options) = Variants $ map (scrutinize ns x) options
  drive ns (Case other options) = inject (drive ns other) where -- TODO: not sure, what if we need to decompose `other'?
    inject (Transient t) = Transient (Case t options) -- kind of traverse
    inject (Variants cs) = Variants $ map f cs
    f (c, t) = (c, Case t options)

scrutinize :: NameSupply -> Name -> (Pat, Expr) -> (Contract, Expr)
scrutinize ns v (Pat c cvars, e) = (Contract v (Pat c fresh), e // sub) where
  fresh = take (length cvars) ns
  sub = zip cvars (map Var fresh)
