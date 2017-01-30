module MiniLogo where

-- Stefan Reindels
-- Evan Steele
-- Will Olsen

-- Define the abstract syntax of MiniLogo as a set of Haskell data types. 
-- You should use built-in types for num, var, and macro. 

import Data.List
type Macro = String
type Var = String
-- Num coming from Prelude

-- prog ::= prog ε | cmd ; prog

-- using type because it is only a list of command items
type Prog = [Cmd]

-- mode ::= mode down | up
data Mode = Up
    | Down
    deriving (Show, Eq)

-- expr ::= var
--        | num
--        | expr + expr

-- just like Mode, just a different set
data Expr = Refer Var
    | Number Int
    | Add Expr Expr
    deriving (Show, Eq)

-- cmd  ::= pen mode
--  |   move ( expr , expr)
--  |   define macro ( var* ) { prog }
--  |   call macro ( expr* )

-- var: https://downloads.haskell.org/~ghc/7.4.1/docs/html/libraries/ghc/Var.html

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving (Show, Eq)

-- Define a MiniLogo macro line (x1,y1,x2,y2) 
-- that starting from anywhere on the canvas draws a line segment 
-- from (x1,y1) to (x2,y2)
--
-- Structure:
-- Takes: [xorigin, yorigin, xdest, ydest]
-- Performs: ( lift pen, navigate to (xorigin, yorigin), drop pen, navigate to (xdest, ydest)  )
--           ( Pen(Mode), Move (xorigin, yorigin), Pen(Mode), Move (xdest, ydest) )
line = Define "line" ["x_origin", "y_origin", "x_dest", "y_dest"] [Pen Up, Move (Refer "x_origin", Refer "y_origin"), Pen Down, Move (Refer "x_destination", Refer "y_destination"), Pen Up]
-- [Pen Up, Move (Refer "x_origin", Refer "y_origin"), 
--Pen Down, Move (Refer "x_destination", Refer "y_destination"), Pen Up]

-- Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) 
-- That draws a big “X” of width w and height h, starting from position (x,y).


-- we need to call 'line' macro, we'll need to call it twice for an X
-- Structure:
-- Takes: [xorigin, yorigin, width, height]

nix = Define "nix" ["x_origin, y_origin, width, height"] [
    Call "line" [Refer "x_origin", Refer "y_origin", Add (Refer "x_origin") (Refer "width"), Add (Refer "y_origin") (Refer "height")],
    Call "line" [Add (Refer "x_origin") (Refer "width"), Refer "y_origin", Refer "x_origin", Add (Refer "y_origin") (Refer "height")]]

steps :: Int -> Prog
steps 0 = []
steps n = steps (n - 1) ++ [Call "line" [Number n, Number n, Number (n-1), Number n], Call "line" [Number (n-1), Number n, Number (n-1), Number (n-1)]]

-- | Gets a list of macros defined
--
--  >>> macros [Define "lol" [] [], Define "foo" [] [], Pen Up, Define "bar" [] [], Call "foo" [], Call "bar" []]
--  ["lol","foo","bar"]
--
--  >>> macros [Call "line" [Number 1,Number 1,Number 0,Number 1],Call "line" [Number 0,Number 1,Number 0,Number 0],Call "line" [Number 2,Number 2,Number 1,Number 2],Call "line" [Number 1,Number 2,Number 1,Number 1]]
--  []
--

macros :: Prog -> [Macro]
macros [] = []
macros ((Define m _ _ ):others) = m:macros others
macros (m:others) = macros others

prettyVars :: [Var] -> String
prettyVars [] = ""
prettyVars (x:xs) = x ++ ", " ++ prettyVars xs

prettyExpr :: [Expr] -> String
prettyExpr [] = ""
prettyExpr (Number n:xs) = show n ++ " " ++ prettyExpr xs
prettyExpr (Refer r:xs) = r ++ " " ++ prettyExpr xs
prettyExpr (Add x y:xs) = prettyExpr [x] ++ " and " ++ prettyExpr [y] ++ "; " ++ " " ++ prettyExpr xs

pretty ((Pen Up):xs) = "pen up;" ++ pretty xs
pretty ((Pen Down):xs) = "pen down; " ++ pretty xs
pretty (Move (ex1, ex2):xs) = "Move (" ++ prettyExpr [ex1, ex2] ++ "); " ++ pretty xs
pretty (Define m v p:xs) = m ++ " (" ++ prettyVars v ++ ") {" ++ pretty p ++ "}; " ++ pretty xs
pretty (Call m exs:xs) = "Macro: " ++ m ++ " [" ++ prettyExpr exs ++ "]; " ++ pretty xs

optE :: Expr -> Expr
optE (Add (Number l) (Number r)) = Number $ l + r
optE otherwise = otherwise
