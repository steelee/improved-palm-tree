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

-- expr	::=	var
--        |	num
--        |	expr + expr

-- just like Mode, just a different set
data Expr = Refer Var
	| Number Int
	| Two_Expr Expr Expr
	deriving (Show, Eq)

-- cmd	::=	pen mode
--	|	move ( expr , expr)
--	|	define macro ( var* ) { prog }
--	|	call macro ( expr* )

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

-- Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) 
-- That draws a big “X” of width w and height h, starting from position (x,y).


-- we need to call 'line' macro, we'll need to call it twice for an X
-- Structure:
-- Takes: [xorigin, yorigin, width, height]
-- Performs :


nix = Define "nix" ["xorigin, yorigin, width, height"]
