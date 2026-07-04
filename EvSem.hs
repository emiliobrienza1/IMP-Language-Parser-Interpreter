---------------------------------------------------------------------
-- EVALUATION SEMANTICS FOR IMP                                  
-- Roy Crole 2026                                            
---------------------------------------------------------------------

module EvSem

where

import AST
import Basic
import Pretty

-- we put the types for configurations in the AST file


-------------------------------------------------------------------
-- Converting the constructors for operators into real functions
-------------------------------------------------------------------

fromIop  :: Iop -> (Int -> Int -> Int)
fromIop Plus  = (+)
fromIop Minus = (-)
fromIop Times = (*)

fromBop :: Bop -> (Int -> Int -> Bool)
-- *** you need to code equality testing for Phase B only ; skip for Phase A --
fromBop Eqq  = (==)
fromBop Le   = (<)
fromBop Gr   = (>)
fromBop LeEq = (<=)
fromBop GrEq = (>=)


----------------------------------------------------------------
--  Code the function evalint for evaluating Integer Expressions
----------------------------------------------------------------

evalint :: IntExpConf -> (Either Error Int)

evalint (Int m, s) = Right m

evalint (Var v, s) = lookUp s v

evalint (IopExp (e1, op, e2), s) = do
    v1 <- evalint (e1, s)
    v2 <- evalint (e2, s)
    Right (fromIop op v1 v2)

                                                                           
-----------------------------------------------------------------------
-- Code the function evalbool for evaluating Boolean Expressions
-----------------------------------------------------------------------

evalbool :: BoolExpConf -> (Either Error Bool)

evalbool (Bool b, s) = Right b

-- *** you will need to code negation and logical and yourself, for Phase B only ***

evalbool (Not b, s) = do
    v <- evalbool (b, s)
    Right (not v)

evalbool (And (b1, b2), s) = do
    v1 <- evalbool (b1, s)
    v2 <- evalbool (b2, s)
    Right (v1 && v2)

evalbool (BopExp (e1, op, e2), s) = do
    v1 <- evalint (e1, s)
    v2 <- evalint (e2, s)
    Right (fromBop op v1 v2)

                                     
--------------------------------------------------------------------------
-- Code the function evalcom for evaluating Commands
--------------------------------------------------------------------------

evalcom :: ComConf -> (Either Error State)

evalcom (Ass (v, e), s) = do
    val <- evalint (e, s)
    Right (update s v val)

evalcom (Seq (c1, c2), s1) = do
    s2 <- evalcom (c1, s1)
    evalcom (c2, s2)

evalcom (If (b, c1, c2), s) = do
    cond <- evalbool (b, s)
    if cond then evalcom (c1, s) else evalcom (c2, s)

evalcom (While (b, c), s1) = do
    cond <- evalbool (b, s1)
    if cond
       then do s2 <- evalcom (c, s1)
               evalcom (While (b, c), s2)
       else Right s1

-- *** you need to code evaluation of repeat loops for Phase B only

evalcom (Repeat (c, b), s1) = do
    s2 <- evalcom (c, s1)
    cond <- evalbool (b, s2)
    if cond
       then Right s2
       else evalcom (Repeat (c, b), s2)