module TreelessData where

type Name = String
data TreelessExpr = Ctr Name [TreelessExpr] | 
data FDef = FDef Name [Name] TreelessExpr deriving (Eq)
