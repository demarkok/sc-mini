module DataIO where

import Data
import DataUtil
import Data.Maybe
import Data.Char

import Data.List
import Text.ParserCombinators.ReadP


-- READ
readVar1 :: ReadS Name
readVar1 i = concat [lex s1 | (",", s1) <- lex i]


instance Read Expr where
  readsPrec _ s = readsExpr s


instance Read Program where
  readsPrec _ s = readProgram s


readExpr :: ReadP Expr
readExpr = readS_to_P readsExpr


readsExpr :: ReadS Expr
readsExpr i = catMaybes [merge n s | (n, s) <- lex i] where
  merge n s 
    | "f" `isPrefixOf` n = Just (Call n args, s1)
    | "lmb" `isPrefixOf` n = Just $ head $ readLambda s
    | "app" `isPrefixOf` n = Just ((args !! 0) :@: (args !! 1), s1)
    | "case" `isPrefixOf` n = Just $ head $ readCase s
    | isUpper $ head n = Just (Ctr n args, s1)
    | isLower $ head n = Just (Var n, s) where
      [(args, s1)] = readArgs s
  merge _ _ = Nothing


 
readArgs :: ReadS [Expr]
readArgs = readP_to_S $ between (char '(') (char ')') (sepBy readExpr (char ','))


readLambda :: ReadS Expr
readLambda = readP_to_S $ do
  skipSpaces
  string "(" ; skipSpaces
  x <- readS_to_P lex ; skipSpaces
  string "->" ; skipSpaces
  expr <- readS_to_P readsExpr ; skipSpaces
  string ")"; skipSpaces
  return $ Lmb x expr



readCase :: ReadS Expr
readCase = readP_to_S $ do
  skipSpaces
  expr <- readS_to_P readsExpr ; skipSpaces
  string "of" ; skipSpaces
  Case expr <$> between (char '[') (char ']') (sepBy readOption (char '|')) where
    readOption = do
      skipSpaces
      pat <- readS_to_P readSPat ; skipSpaces
      string "=>" ; skipSpaces
      option <- readS_to_P readsExpr ; skipSpaces
      return (pat, option)
  

readVars :: ReadS [Name]
readVars = readP_to_S $ between (char '(') (char ')') (sepBy (readS_to_P lex) (char ','))


readFDef :: ReadS FDef
readFDef i = [ (FDef n vars body, s4) |
  (n@('f':_), s) <- lex i,
  (vars, s1) <- readVars s,
  ("=", s2) <- lex s1,
  (body, s3) <- readsExpr s2,
  (";", s4) <- lex s3]


readSPat :: ReadS Pat
readSPat i = [(Pat n vars, s2)|
  (n, s) <- lex i,
  (vars, s2) <- readVars s]


readProgram s = [readP1 (Program []) s]


readP1 p@(Program fs) s = next (readFDef s) where
  next [(f, s1)] = readP1 (Program (fs++[f])) s1
  next _ = (p, s)


-- SHOW


printTree :: Int -> Graph Conf -> String
printTree n t = unlines $ take n $ pprintTree "" "" t


pprintTree :: String -> String -> Graph Conf -> [String]
pprintTree indent msg (Node expr next) = make next where
  make (Fold _ ren) = (indent ++ msg) : [indent ++ "|__" ++  (show expr) ++ "__↑" ++ (show ren)]
  make Stop = (indent ++ msg) : [indent ++ "|__" ++  (show expr)]
  make (Transient t) = (indent ++ msg) : (indent ++ "|__" ++ show expr) : (pprintTree (indent ++ " ") "|" t)
  make (Decompose ts) = (indent ++ msg) :  (indent ++ "|__" ++ show expr): (concat (map (pprintTree (indent ++ " ") "|") ts))
  make (Variants cs) =
    (indent ++ msg) :  (indent ++ "|__" ++  show expr) : (concat (map (\(x, t) -> pprintTree (indent ++ " ") ("?" ++ show x) t) cs))


instance Show Expr where
  show (Ctr "Nil" []) = "Nil"
  show (Ctr "Cons" [Ctr "B" [], Ctr "Nil" []]) = "B"
  -- show (Ctr "Cons" [Ctr "A" [], (Ctr "Cons" [Ctr "B" [], Ctr "Nil" []])]) = "``AB\'\'"
  -- show (Ctr "Cons" [Ctr "A" [], (Ctr "Cons" [Ctr "A" [], (Ctr "Cons" [Ctr "B" [], Ctr "Nil" []])])]) = "``AAB\'\'"
  show (Ctr "Cons" [x, y]) = (show x) ++ ":" ++ (show y)
  show (Ctr "A" []) = "A"
  show (Ctr "B" []) = "B"
  show (Var n) = n
  show (Ctr n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
  show (Call n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
  show (Let (v, e1) e2) = "let " ++ v ++ " = " ++ (show e1) ++ " in " ++ (show e2)
  show (Case e options) = "case " ++ (show e) ++ " of " ++ "[" ++ (intercalate " | " (map f options)) ++ "]" where
    f (pat, option) = show pat ++ " => " ++ show option
  show (Lmb x e) = "lmb(" ++ show x ++ " -> " ++ show e ++ ")"
  show (a :@: b) = "app(" ++ show a ++ "," ++ show b ++ ")"



instance Show FDef where
  show (FDef n args body) = n ++ "(" ++ intercalate ", " args ++ ") = " ++ (show body) ++ ";"


instance Show Pat where
  show (Pat "Nil" vs) = "Nil"
  show (Pat "Cons" [v1, v2]) = v1 ++ ":" ++ v2
  show (Pat cn vs) = cn ++ "(" ++ intercalate "," vs ++ ")"


instance Show Contract where
  show (Contract n p) = n ++ " == " ++ (show p)


instance Show Program where
  show (Program fs) = intercalate "\n" $ (map show fs)


instance Show a => Show (Step a) where
  show (Transient a) = "=> " ++ (show a)
  show (Variants vs) = intercalate "\n" $ map (\(c, e) -> (show c) ++ " => " ++ (show e)) vs
  show Stop = "!"
  show (Decompose ds) = show ds
  show (Fold e _) = "↑" ++ (show e)