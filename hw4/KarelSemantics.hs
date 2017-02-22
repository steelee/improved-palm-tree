
-- Stefan Reindel - reindels
-- Evan Steele    - steelee
-- Will Olsen     - olsenw

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r     = not $ test t w r
test (Facing c) w r  = (getFacing r) == c 
test (Clear d) w r   = isClear (relativePos d r) w
test (Beeper) w r    = hasBeeper (getPos r) w
test (Empty) w r     = isEmpty r 

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                        in if (getBag r) > 0
                              then OK (incBeeper p w) (decBag r) 
                              else Error ("No beeper to put.")	
stmt Move _ w r      = let p = neighbor (getFacing r) (getPos r)
                        in if isClear p w
			      then OK w (setPos p r)
			      else Error ("Blocked at: " ++ show p)	
stmt (Turn d) _ w r      = OK w (setFacing (cardTurn d (getFacing r) ) r)			      	
stmt (Block []) _ w r = OK w r

stmt (Block (s:xs)) d w r = case stmt s d w r of
			OK w' r' -> stmt (Block xs) d w' r'  
			Done r' -> Done r'
		        Error e  -> Error e
-- If Test Stmt Stmt
stmt (If c t e) d w r = if (test c w r) 
			then stmt t d w r
			else stmt e d w r
   
stmt (Call m) d w r = case (lookup m d) of
                        Just s -> stmt s d w r
                        Nothing -> Error ("Undefined macro: " ++ m)

stmt (While c s) d w r = if (test c w r) 
                        then case (stmt s d w r) of 
                                (OK w' r') -> stmt (While c s) d w' r'
				Done r' -> Done r'
                                Error e    -> Error e
                        else OK w r

stmt (Iterate i s) d w r = if (i > 0)
                        then case (stmt s d w r) of 
                                (OK w' r') -> stmt (Iterate (i-1) s) d w' r'
				Done r' -> Done r'
                                Error e -> Error e
                        else (OK w r) 

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
