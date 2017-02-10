module HW3 where

import MiniMiniLogo
import Render

--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Down) (s, p)    = ((Down, p), Nothing)
cmd (Pen Up) (s, p)      = ((Up, p), Nothing)
cmd (Move i j) (Up, p)   = ((Up, (i,j)), Nothing)
cmd (Move i j) (Down, p) = ((Down, (i,j)), Just (p, (i,j)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] s = (s, [])
-- we need a case structure to deal with these movements
prog (c:cs) st = case cmd c st of
	-- lambda function: pull the list of commands apart
	(s, Just l) -> (\(st,cs) -> (st,l:cs)) $ prog cs s

        (s, _) -> prog cs s


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = [Pen Up, Move 0 39, Pen Down, Pen Up, Move 0 38, Pen Down, Pen Up, Move 2 38, Pen Down, Move 78 38, Pen Up, Move 0 37, Pen Down, Pen Up, Move 2 37, Pen Down, Move 3 37, Pen Up, Move 75 37, Pen Down, Move 78 37, Pen Up, Move 0 36, Pen Down, Pen Up, Move 2 36, Pen Down, Move 3 36, Pen Up, Move 75 36, Pen Down, Move 78 36, Pen Up, Move 0 35, Pen Down, Pen Up, Move 2 35, Pen Down, Move 3 35, Pen Up, Move 75 35, Pen Down, Move 78 35, Pen Up, Move 0 34, Pen Down, Pen Up, Move 2 34, Pen Down, Move 3 34, Pen Up, Move 75 34, Pen Down, Move 78 34, Pen Up, Move 0 33, Pen Down, Pen Up, Move 2 33, Pen Down, Move 3 33, Pen Up, Move 33 33, Pen Down, Move 47 33, Pen Up, Move 75 33, Pen Down, Move 78 33, Pen Up, Move 0 32, Pen Down, Pen Up, Move 2 32, Pen Down, Move 3 32, Pen Up, Move 28 32, Pen Down, Move 34 32, Pen Up, Move 44 32, Pen Down, Move 51 32, Pen Up, Move 75 32, Pen Down, Move 78 32, Pen Up, Move 0 31, Pen Down, Pen Up, Move 2 31, Pen Down, Move 3 31, Pen Up, Move 24 31, Pen Down, Move 29 31, Pen Up, Move 51 31, Pen Down, Move 54 31, Pen Up, Move 75 31, Pen Down, Move 78 31, Pen Up, Move 0 30, Pen Down, Pen Up, Move 2 30, Pen Down, Move 3 30, Pen Up, Move 23 30, Pen Down, Move 26 30, Pen Up, Move 52 30, Pen Down, Move 57 30, Pen Up, Move 75 30, Pen Down, Move 78 30, Pen Up, Move 0 29, Pen Down, Pen Up, Move 2 29, Pen Down, Move 3 29, Pen Up, Move 20 29, Pen Down, Move 21 29, Pen Up, Move 57 29, Pen Down, Move 60 29, Pen Up, Move 75 29, Pen Down, Move 78 29, Pen Up, Move 0 28, Pen Down, Pen Up, Move 2 28, Pen Down, Move 3 28, Pen Up, Move 18 28, Pen Down, Move 20 28, Pen Up, Move 59 28, Pen Down, Move 60 28, Pen Up, Move 75 28, Pen Down, Move 78 28, Pen Up, Move 0 27, Pen Down, Pen Up, Move 2 27, Pen Down, Move 3 27, Pen Up, Move 16 27, Pen Down, Move 20 27, Pen Up, Move 28 27, Pen Down, Move 29 27, Pen Up, Move 44 27, Pen Down, Move 46 27, Pen Up, Move 60 27, Pen Down, Move 62 27, Pen Up, Move 75 27, Pen Down, Move 78 27, Pen Up, Move 0 26, Pen Down, Pen Up, Move 2 26, Pen Down, Move 3 26, Pen Up, Move 16 26, Pen Down, Move 18 26, Pen Up, Move 28 26, Pen Down, Move 29 26, Pen Up, Move 44 26, Pen Down, Move 46 26, Pen Up, Move 60 26, Pen Down, Move 64 26, Pen Up, Move 75 26, Pen Down, Move 78 26, Pen Up, Move 0 25, Pen Down, Pen Up, Move 2 25, Pen Down, Move 3 25, Pen Up, Move 13 25, Pen Down, Move 16 25, Pen Up, Move 28 25, Pen Down, Move 29 25, Pen Up, Move 44 25, Pen Down, Move 46 25, Pen Up, Move 62 25, Pen Down, Move 65 25, Pen Up, Move 75 25, Pen Down, Move 78 25, Pen Up, Move 0 24, Pen Down, Pen Up, Move 2 24, Pen Down, Move 3 24, Pen Up, Move 13 24, Pen Down, Move 15 24, Pen Up, Move 28 24, Pen Down, Move 29 24, Pen Up, Move 44 24, Pen Down, Move 46 24, Pen Up, Move 64 24, Pen Down, Move 65 24, Pen Up, Move 75 24, Pen Down, Move 78 24, Pen Up, Move 0 23, Pen Down, Pen Up, Move 2 23, Pen Down, Move 3 23, Pen Up, Move 13 23, Pen Down, Move 15 23, Pen Up, Move 28 23, Pen Down, Move 29 23, Pen Up, Move 44 23, Pen Down, Move 46 23, Pen Up, Move 64 23, Pen Down, Move 67 23, Pen Up, Move 75 23, Pen Down, Move 78 23, Pen Up, Move 0 22, Pen Down, Pen Up, Move 2 22, Pen Down, Move 3 22, Pen Up, Move 11 22, Pen Down, Move 15 22, Pen Up, Move 28 22, Pen Down, Move 29 22, Pen Up, Move 44 22, Pen Down, Move 46 22, Pen Up, Move 64 22, Pen Down, Move 67 22, Pen Up, Move 75 22, Pen Down, Move 78 22, Pen Up, Move 0 21, Pen Down, Pen Up, Move 2 21, Pen Down, Move 3 21, Pen Up, Move 11 21, Pen Down, Move 13 21, Pen Up, Move 44 21, Pen Down, Move 46 21, Pen Up, Move 65 21, Pen Down, Move 67 21, Pen Up, Move 75 21, Pen Down, Move 78 21, Pen Up, Move 0 20, Pen Down, Pen Up, Move 2 20, Pen Down, Move 3 20, Pen Up, Move 11 20, Pen Down, Move 13 20, Pen Up, Move 65 20, Pen Down, Move 67 20, Pen Up, Move 75 20, Pen Down, Move 78 20, Pen Up, Move 0 19, Pen Down, Pen Up, Move 2 19, Pen Down, Move 3 19, Pen Up, Move 11 19, Pen Down, Move 13 19, Pen Up, Move 65 19, Pen Down, Move 67 19, Pen Up, Move 75 19, Pen Down, Move 78 19, Pen Up, Move 0 18, Pen Down, Pen Up, Move 2 18, Pen Down, Move 3 18, Pen Up, Move 11 18, Pen Down, Move 13 18, Pen Up, Move 65 18, Pen Down, Move 67 18, Pen Up, Move 75 18, Pen Down, Move 78 18, Pen Up, Move 0 17, Pen Down, Pen Up, Move 2 17, Pen Down, Move 3 17, Pen Up, Move 11 17, Pen Down, Move 15 17, Pen Up, Move 64 17, Pen Down, Move 67 17, Pen Up, Move 75 17, Pen Down, Move 78 17, Pen Up, Move 0 16, Pen Down, Pen Up, Move 2 16, Pen Down, Move 3 16, Pen Up, Move 13 16, Pen Down, Move 15 16, Pen Up, Move 23 16, Pen Down, Move 24 16, Pen Up, Move 64 16, Pen Down, Move 67 16, Pen Up, Move 75 16, Pen Down, Move 78 16, Pen Up, Move 0 15, Pen Down, Pen Up, Move 2 15, Pen Down, Move 3 15, Pen Up, Move 13 15, Pen Down, Move 15 15, Pen Up, Move 23 15, Pen Down, Move 29 15, Pen Up, Move 47 15, Pen Down, Move 52 15, Pen Up, Move 64 15, Pen Down, Move 65 15, Pen Up, Move 75 15, Pen Down, Move 78 15, Pen Up, Move 0 14, Pen Down, Pen Up, Move 2 14, Pen Down, Move 3 14, Pen Up, Move 13 14, Pen Down, Move 16 14, Pen Up, Move 28 14, Pen Down, Move 33 14, Pen Up, Move 44 14, Pen Down, Move 49 14, Pen Up, Move 64 14, Pen Down, Move 65 14, Pen Up, Move 75 14, Pen Down, Move 78 14, Pen Up, Move 0 13, Pen Down, Pen Up, Move 2 13, Pen Down, Move 3 13, Pen Up, Move 16 13, Pen Down, Move 18 13, Pen Up, Move 36 13, Pen Down, Move 41 13, Pen Up, Move 60 13, Pen Down, Move 64 13, Pen Up, Move 75 13, Pen Down, Move 78 13, Pen Up, Move 0 12, Pen Down, Pen Up, Move 2 12, Pen Down, Move 3 12, Pen Up, Move 16 12, Pen Down, Move 20 12, Pen Up, Move 60 12, Pen Down, Move 62 12, Pen Up, Move 75 12, Pen Down, Move 78 12, Pen Up, Move 0 11, Pen Down, Pen Up, Move 2 11, Pen Down, Move 3 11, Pen Up, Move 18 11, Pen Down, Move 20 11, Pen Up, Move 59 11, Pen Down, Move 60 11, Pen Up, Move 75 11, Pen Down, Move 78 11, Pen Up, Move 0 10, Pen Down, Pen Up, Move 2 10, Pen Down, Move 3 10, Pen Up, Move 20 10, Pen Down, Move 21 10, Pen Up, Move 57 10, Pen Down, Move 60 10, Pen Up, Move 75 10, Pen Down, Move 78 10, Pen Up, Move 0 9, Pen Down, Pen Up, Move 2 9, Pen Down, Move 3 9, Pen Up, Move 23 9, Pen Down, Move 26 9, Pen Up, Move 52 9, Pen Down, Move 57 9, Pen Up, Move 75 9, Pen Down, Move 78 9, Pen Up, Move 0 8, Pen Down, Pen Up, Move 2 8, Pen Down, Move 3 8, Pen Up, Move 24 8, Pen Down, Move 29 8, Pen Up, Move 51 8, Pen Down, Move 54 8, Pen Up, Move 75 8, Pen Down, Move 78 8, Pen Up, Move 0 7, Pen Down, Pen Up, Move 2 7, Pen Down, Move 3 7, Pen Up, Move 28 7, Pen Down, Move 34 7, Pen Up, Move 44 7, Pen Down, Move 51 7, Pen Up, Move 75 7, Pen Down, Move 78 7, Pen Up, Move 0 6, Pen Down, Pen Up, Move 2 6, Pen Down, Move 3 6, Pen Up, Move 33 6, Pen Down, Move 47 6, Pen Up, Move 75 6, Pen Down, Move 78 6, Pen Up, Move 0 5, Pen Down, Pen Up, Move 2 5, Pen Down, Move 3 5, Pen Up, Move 75 5, Pen Down, Move 78 5, Pen Up, Move 0 4, Pen Down, Pen Up, Move 2 4, Pen Down, Move 3 4, Pen Up, Move 75 4, Pen Down, Move 78 4, Pen Up, Move 0 3, Pen Down, Pen Up, Move 2 3, Pen Down, Move 3 3, Pen Up, Move 75 3, Pen Down, Move 78 3, Pen Up, Move 0 2, Pen Down, Pen Up, Move 2 2, Pen Down, Move 3 2, Pen Up, Move 75 2, Pen Down, Move 78 2, Pen Up, Move 0 1, Pen Down, Pen Up, Move 2 1, Pen Down, Move 78 1]
--amazing = [Pen Down, Move 0 39, Pen Up, Pen Down, Move 0 38, Pen Up, Pen Down, Move 2 38, Pen Up, Move 78 38, Pen Down, Move 0 37, Pen Up, Pen Down, Move 2 37, Pen Up, Move 3 37, Pen Down, Move 75 37, Pen Up, Move 78 37, Pen Down, Move 0 36, Pen Up, Pen Down, Move 2 36, Pen Up, Move 3 36, Pen Down, Move 75 36, Pen Up, Move 78 36, Pen Down, Move 0 35, Pen Up, Pen Down, Move 2 35, Pen Up, Move 3 35, Pen Down, Move 75 35, Pen Up, Move 78 35, Pen Down, Move 0 34, Pen Up, Pen Down, Move 2 34, Pen Up, Move 3 34, Pen Down, Move 75 34, Pen Up, Move 78 34, Pen Down, Move 0 33, Pen Up, Pen Down, Move 2 33, Pen Up, Move 3 33, Pen Down, Move 33 33, Pen Up, Move 47 33, Pen Down, Move 75 33, Pen Up, Move 78 33, Pen Down, Move 0 32, Pen Up, Pen Down, Move 2 32, Pen Up, Move 3 32, Pen Down, Move 28 32, Pen Up, Move 34 32, Pen Down, Move 44 32, Pen Up, Move 51 32, Pen Down, Move 75 32, Pen Up, Move 78 32, Pen Down, Move 0 31, Pen Up, Pen Down, Move 2 31, Pen Up, Move 3 31, Pen Down, Move 24 31, Pen Up, Move 29 31, Pen Down, Move 51 31, Pen Up, Move 54 31, Pen Down, Move 75 31, Pen Up, Move 78 31, Pen Down, Move 0 30, Pen Up, Pen Down, Move 2 30, Pen Up, Move 3 30, Pen Down, Move 23 30, Pen Up, Move 26 30, Pen Down, Move 52 30, Pen Up, Move 57 30, Pen Down, Move 75 30, Pen Up, Move 78 30, Pen Down, Move 0 29, Pen Up, Pen Down, Move 2 29, Pen Up, Move 3 29, Pen Down, Move 20 29, Pen Up, Move 21 29, Pen Down, Move 57 29, Pen Up, Move 60 29, Pen Down, Move 75 29, Pen Up, Move 78 29, Pen Down, Move 0 28, Pen Up, Pen Down, Move 2 28, Pen Up, Move 3 28, Pen Down, Move 18 28, Pen Up, Move 20 28, Pen Down, Move 59 28, Pen Up, Move 60 28, Pen Down, Move 75 28, Pen Up, Move 78 28, Pen Down, Move 0 27, Pen Up, Pen Down, Move 2 27, Pen Up, Move 3 27, Pen Down, Move 16 27, Pen Up, Move 20 27, Pen Down, Move 28 27, Pen Up, Move 29 27, Pen Down, Move 44 27, Pen Up, Move 46 27, Pen Down, Move 60 27, Pen Up, Move 62 27, Pen Down, Move 75 27, Pen Up, Move 78 27, Pen Down, Move 0 26, Pen Up, Pen Down, Move 2 26, Pen Up, Move 3 26, Pen Down, Move 16 26, Pen Up, Move 18 26, Pen Down, Move 28 26, Pen Up, Move 29 26, Pen Down, Move 44 26, Pen Up, Move 46 26, Pen Down, Move 60 26, Pen Up, Move 64 26, Pen Down, Move 75 26, Pen Up, Move 78 26, Pen Down, Move 0 25, Pen Up, Pen Down, Move 2 25, Pen Up, Move 3 25, Pen Down, Move 13 25, Pen Up, Move 16 25, Pen Down, Move 28 25, Pen Up, Move 29 25, Pen Down, Move 44 25, Pen Up, Move 46 25, Pen Down, Move 62 25, Pen Up, Move 65 25, Pen Down, Move 75 25, Pen Up, Move 78 25, Pen Down, Move 0 24, Pen Up, Pen Down, Move 2 24, Pen Up, Move 3 24, Pen Down, Move 13 24, Pen Up, Move 15 24, Pen Down, Move 28 24, Pen Up, Move 29 24, Pen Down, Move 44 24, Pen Up, Move 46 24, Pen Down, Move 64 24, Pen Up, Move 65 24, Pen Down, Move 75 24, Pen Up, Move 78 24, Pen Down, Move 0 23, Pen Up, Pen Down, Move 2 23, Pen Up, Move 3 23, Pen Down, Move 13 23, Pen Up, Move 15 23, Pen Down, Move 28 23, Pen Up, Move 29 23, Pen Down, Move 44 23, Pen Up, Move 46 23, Pen Down, Move 64 23, Pen Up, Move 67 23, Pen Down, Move 75 23, Pen Up, Move 78 23, Pen Down, Move 0 22, Pen Up, Pen Down, Move 2 22, Pen Up, Move 3 22, Pen Down, Move 11 22, Pen Up, Move 15 22, Pen Down, Move 28 22, Pen Up, Move 29 22, Pen Down, Move 44 22, Pen Up, Move 46 22, Pen Down, Move 64 22, Pen Up, Move 67 22, Pen Down, Move 75 22, Pen Up, Move 78 22, Pen Down, Move 0 21, Pen Up, Pen Down, Move 2 21, Pen Up, Move 3 21, Pen Down, Move 11 21, Pen Up, Move 13 21, Pen Down, Move 44 21, Pen Up, Move 46 21, Pen Down, Move 65 21, Pen Up, Move 67 21, Pen Down, Move 75 21, Pen Up, Move 78 21, Pen Down, Move 0 20, Pen Up, Pen Down, Move 2 20, Pen Up, Move 3 20, Pen Down, Move 11 20, Pen Up, Move 13 20, Pen Down, Move 65 20, Pen Up, Move 67 20, Pen Down, Move 75 20, Pen Up, Move 78 20, Pen Down, Move 0 19, Pen Up, Pen Down, Move 2 19, Pen Up, Move 3 19, Pen Down, Move 11 19, Pen Up, Move 13 19, Pen Down, Move 65 19, Pen Up, Move 67 19, Pen Down, Move 75 19, Pen Up, Move 78 19, Pen Down, Move 0 18, Pen Up, Pen Down, Move 2 18, Pen Up, Move 3 18, Pen Down, Move 11 18, Pen Up, Move 13 18, Pen Down, Move 65 18, Pen Up, Move 67 18, Pen Down, Move 75 18, Pen Up, Move 78 18, Pen Down, Move 0 17, Pen Up, Pen Down, Move 2 17, Pen Up, Move 3 17, Pen Down, Move 11 17, Pen Up, Move 15 17, Pen Down, Move 64 17, Pen Up, Move 67 17, Pen Down, Move 75 17, Pen Up, Move 78 17, Pen Down, Move 0 16, Pen Up, Pen Down, Move 2 16, Pen Up, Move 3 16, Pen Down, Move 13 16, Pen Up, Move 15 16, Pen Down, Move 23 16, Pen Up, Move 24 16, Pen Down, Move 64 16, Pen Up, Move 67 16, Pen Down, Move 75 16, Pen Up, Move 78 16, Pen Down, Move 0 15, Pen Up, Pen Down, Move 2 15, Pen Up, Move 3 15, Pen Down, Move 13 15, Pen Up, Move 15 15, Pen Down, Move 23 15, Pen Up, Move 29 15, Pen Down, Move 47 15, Pen Up, Move 52 15, Pen Down, Move 64 15, Pen Up, Move 65 15, Pen Down, Move 75 15, Pen Up, Move 78 15, Pen Down, Move 0 14, Pen Up, Pen Down, Move 2 14, Pen Up, Move 3 14, Pen Down, Move 13 14, Pen Up, Move 16 14, Pen Down, Move 28 14, Pen Up, Move 33 14, Pen Down, Move 44 14, Pen Up, Move 49 14, Pen Down, Move 64 14, Pen Up, Move 65 14, Pen Down, Move 75 14, Pen Up, Move 78 14, Pen Down, Move 0 13, Pen Up, Pen Down, Move 2 13, Pen Up, Move 3 13, Pen Down, Move 16 13, Pen Up, Move 18 13, Pen Down, Move 36 13, Pen Up, Move 41 13, Pen Down, Move 60 13, Pen Up, Move 64 13, Pen Down, Move 75 13, Pen Up, Move 78 13, Pen Down, Move 0 12, Pen Up, Pen Down, Move 2 12, Pen Up, Move 3 12, Pen Down, Move 16 12, Pen Up, Move 20 12, Pen Down, Move 60 12, Pen Up, Move 62 12, Pen Down, Move 75 12, Pen Up, Move 78 12, Pen Down, Move 0 11, Pen Up, Pen Down, Move 2 11, Pen Up, Move 3 11, Pen Down, Move 18 11, Pen Up, Move 20 11, Pen Down, Move 59 11, Pen Up, Move 60 11, Pen Down, Move 75 11, Pen Up, Move 78 11, Pen Down, Move 0 10, Pen Up, Pen Down, Move 2 10, Pen Up, Move 3 10, Pen Down, Move 20 10, Pen Up, Move 21 10, Pen Down, Move 57 10, Pen Up, Move 60 10, Pen Down, Move 75 10, Pen Up, Move 78 10, Pen Down, Move 0 9, Pen Up, Pen Down, Move 2 9, Pen Up, Move 3 9, Pen Down, Move 23 9, Pen Up, Move 26 9, Pen Down, Move 52 9, Pen Up, Move 57 9, Pen Down, Move 75 9, Pen Up, Move 78 9, Pen Down, Move 0 8, Pen Up, Pen Down, Move 2 8, Pen Up, Move 3 8, Pen Down, Move 24 8, Pen Up, Move 29 8, Pen Down, Move 51 8, Pen Up, Move 54 8, Pen Down, Move 75 8, Pen Up, Move 78 8, Pen Down, Move 0 7, Pen Up, Pen Down, Move 2 7, Pen Up, Move 3 7, Pen Down, Move 28 7, Pen Up, Move 34 7, Pen Down, Move 44 7, Pen Up, Move 51 7, Pen Down, Move 75 7, Pen Up, Move 78 7, Pen Down, Move 0 6, Pen Up, Pen Down, Move 2 6, Pen Up, Move 3 6, Pen Down, Move 33 6, Pen Up, Move 47 6, Pen Down, Move 75 6, Pen Up, Move 78 6, Pen Down, Move 0 5, Pen Up, Pen Down, Move 2 5, Pen Up, Move 3 5, Pen Down, Move 75 5, Pen Up, Move 78 5, Pen Down, Move 0 4, Pen Up, Pen Down, Move 2 4, Pen Up, Move 3 4, Pen Down, Move 75 4, Pen Up, Move 78 4, Pen Down, Move 0 3, Pen Up, Pen Down, Move 2 3, Pen Up, Move 3 3, Pen Down, Move 75 3, Pen Up, Move 78 3, Pen Down, Move 0 2, Pen Up, Pen Down, Move 2 2, Pen Up, Move 3 2, Pen Down, Move 75 2, Pen Up, Move 78 2, Pen Down, Move 0 1, Pen Up, Pen Down, Move 2 1, Pen Up, Move 78 1]
mario :: Prog
mario = [Pen Up, Move (2*2) (2*2), 
           -- Boot
           Pen Down,
           Move (6*2) (2*2),
           Move (6*2) (4*2),
           Move (3*2) (4*2),
           Move (3*2) (3*2),
           Move (2*2) (3*2),
           Move (2*2) (2*2),
           Pen Up,
           -- other boot
           Move (10*2) (2*2),
           Pen Down,
           Move (10*2) (4*2),
           Move (13*2) (4*2),
           Move (13*2) (3*2),
           Move (14*2) (3*2),
           Move (14*2) (2*2),
           Move (10*2) (2*2),
           Pen Up,
           -- Overalls
           Move (7*2) (5*2),
           Pen Down,
           Move (7*2) (4*2),
           Move (4*2) (4*2),
           Move (4*2) (6*2),
           Move (5*2) (6*2),
           Move (5*2) (8*2),
           Move (7*2) (8*2),
           Move (7*2) (7*2),
           Move (6*2) (7*2),
           Move (6*2) (11*2),
           Move (7*2) (11*2),
           Move (7*2) (9*2),
           Move (9*2) (9*2),
           Move (9*2) (11*2),
           Move (10*2) (11*2),
           Move (10*2) (7*2),
           Move (9*2) (7*2),
           Move (9*2) (8*2),
           Move (11*2) (8*2),
           Move (11*2) (6*2),
           Move (12*2) (6*2),
           Move (12*2) (4*2),
           Move (9*2) (4*2),
           Move (9*2) (5*2),
           Move (7*2) (5*2),
           Pen Up,
           -- left Arm
           Move (2*2) (8*2),
           Pen Down,
           Move (2*2) (5*2),
           Move (4*2) (5*2),
           Move (4*2) (6*2),
           Move (5*2) (6*2),
           Move (5*2) (7*2),
           Move (4*2) (7*2),
           Move (4*2) (8*2),
           Move (2*2) (8*2),
           Move (2*2) (9*2),
           Move (3*2) (9*2),
           Move (3*2) (10*2),
           Move (4*2) (10*2),
           Move (4*2) (11*2),
           Move (5*2) (11*2),
           Pen Up,
           -- Right Arm
           Move (14*2) (8*2),
           Pen Down,
           Move (14*2) (5*2),
           Move (12*2) (5*2),
           Move (12*2) (6*2),
           Move (11*2) (6*2),
           Move (11*2) (7*2),
           Move (12*2) (7*2),
           Move (12*2) (8*2),
           Move (14*2) (8*2),
           Move (14*2) (9*2),
           Move (13*2) (9*2),
           Move (13*2) (10*2),
           Move (10*2) (10*2),
           Pen Up,
           -- Head
           Move (5*2) (11*2),
           Pen Down,
           Move (12*2) (11*2),
           Move (12*2) (12*2), 
           Move (13*2) (12*2),
           Move (13*2) (13*2),
           Pen Up,
           Move (12*2) (12*2),
           Pen Down,
           Move (9*2) (12*2),
           Move (9*2) (13*2),
           Move (10*2) (13*2),
           Move (10*2) (16*2),
           Move (9*2) (16*2),
           Move (9*2) (14*2),
           Move (11*2) (14*2),
           Move (11*2) (13*2),
           Move (14*2) (13*2),
           Move (14*2) (14*2),
           Move (13*2) (14*2),
           Move (13*2) (15*2),
           Move (11*2) (15*2),
           Move (11*2) (16*2),
           Move (12*2) (16*2),
           Move (12*2) (17*2),
           Move (9*2) (17*2),
           Move (9*2) (18*2),
           Move (5*2) (18*2),
           Move (5*2) (17*2),
           Move (4*2) (17*2),
           Move (4*2) (16*2),
           Move (12*2) (16*2),
           Move (7*2) (16*2),
           Move (7*2) (15*2),
           Move (6*2) (15*2),
           Move (6*2) (14*2),
           Move (7*2) (14*2),
           Move (7*2) (13*2),
           Move (5*2) (13*2),
           Move (4*2) (13*2),
           Move (4*2) (16*2),
           Move (4*2) (15*2),
           Move (3*2) (15*2),
           Move (3*2) (12*2),
           Move (5*2) (12*2),
           Move (5*2) (11*2),
           Move (5*2) (15*2),
           Move (4*2) (15*2),
           Pen Up
           ]