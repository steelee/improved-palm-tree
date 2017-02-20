module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r     = neg (test t w r)
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
                              else Error ("No beeper to place: " ++ show p)	
stmt Move _ w r      = let p = relativePos d r
                        in if isClear p w
			      then OK (setPos p r)
			      else Error ("Blocked at: " ++ show p)	
stmt (Turn d) _ _ r      = OK (setFacing (cardTurn d (getFacing r) ) r)			      	
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
