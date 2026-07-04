--------------------------------------------------------------------
-- HASKELL IMP pretty PRINTER                
-- Roy Crole 2026                                        
--------------------------------------------------------------------

module Pretty where

import Basic
import AST -- **** NOTE you MUST complete AST.hs before Pretty.hs will run ****

-- pretty print x where x :: Com, IntExp or BoolExp 
-- applying parsecom, parseiexp or parsebexp to some IMP code produces Right x, the input to usepp 
usepp (Right x) = pp x

class Pretty a where 
      pp ::  a -> String     -- pretty printer 
      precedence :: a -> Int     -- precedence for the constructors 
      bracket :: Int ->  a -> String 
     
--  test precedence and on lower precedence wrap in brackets 
      bracket n e 
        | (precedence e) >= n = pp e
        | otherwise           = "("++(pp e)++")"


----------------------------
-- Code a pretty Printer  --
----------------------------


-- a pretty printer for commands 

instance Pretty Com where 
   pp (Ass (v,e)) =  v ++ "=" ++ (  bracket 4 e)
   pp (If(e,c1,c2)) =  "if " ++ (  bracket 3 e) ++ " then " ++ (  bracket 3 c1) ++ " else " ++ (  bracket 3 c2)
   pp (While(e,c)) = "while " ++ (  bracket 2 e) ++ " do " ++ (bracket 2 c)
   -- *** code for repeat loops Phase B only 
   pp (Repeat(c,e)) = "repeat " ++ (bracket 2 c) ++ " until " ++ (bracket 2 e)
   pp (Seq (c1,c2)) = (  bracket 1 c1)++ " ; " ++ (  bracket 1 c2)
   
   precedence (Ass (v,ie)) = 5
   precedence (If (be,co,co')) = 4
   precedence (While (co,co')) = 3
   -- *** precedence for repeat loops Phase B only 
   -- *** precedence (?) = 2 Phase B only 
   precedence (Repeat(co,be)) = 2
   precedence (Seq (co,co')) = 1

-- a pretty printer for integer  expressions 

instance Pretty IntExp where 
      pp (Int z)  = show z
      pp (Var v)  = v
      pp (IopExp(e1,Plus,e2)) =  case e2 of
                                  IopExp(x,Plus,y) -> 
                                   (  bracket 1 e1) ++ "+" ++ ("(" ++ (  bracket 1 x) ++ "+" ++ (  bracket 1 y) ++ ")")
                                  IopExp(x,Minus,y) -> 
                                   (  bracket 1 e1) ++ "-" ++  ("(" ++ (  bracket 1 x) ++ "-" ++ (  bracket 1 y) ++ ")")
                                  _ -> (  bracket 1 e1) ++ "+" ++ (  bracket 1 e2)

      pp (IopExp(e1,Minus,e2)) =  case e2 of
                                   IopExp(x,Minus,y) -> 
                                    (  bracket 1 e1) ++ "-" ++ ("(" ++ (  bracket 1 x) ++ "-" ++ (  bracket 1 y) ++ ")")
                                   IopExp(x,Plus,y) -> 
                                    (  bracket 1 e1) ++ "+" ++ ("(" ++ (  bracket 1 x) ++ "+" ++ (  bracket 1 y) ++ ")")
                                   _ -> (  bracket 1 e1) ++ "-" ++ (  bracket 1 e2)
                                 
      pp (IopExp(e1,Times,e2)) = (  bracket 3 e1) ++ "*" ++ (  bracket 3 e2)
           
      precedence (Int z) = 4
      precedence (Var v) = 4
      precedence (IopExp (ie,Times,ie')) = 3
      precedence (IopExp (ie,Plus,ie')) = 1
      precedence (IopExp (ie,Minus,ie')) = 1

-- a pretty printer for Boolean expressions

instance Pretty BoolExp where 
      pp (Bool b) = if b then "true" else "false"
      -- *** code for Boolean negation Phase B only 
      pp (Not b) = "not " ++ (bracket 3 b)
      -- *** code for Boolean and  Phase B only 
      pp (And(b1,b2)) = (bracket 2 b1) ++ " & " ++ (bracket 2 b2)
      -- *** code for integer equality Phase B only 
      pp (BopExp(e1,Eqq,e2)) = (bracket 1 e1) ++ "==" ++ (bracket 1 e2)
      pp (BopExp(e1,Le,e2)) = (bracket 1 e1) ++ "<" ++ (bracket 1 e2)
      pp (BopExp(e1,Gr,e2)) = (bracket 1 e1) ++ ">" ++ (bracket 1 e2)
      pp (BopExp(e1,LeEq,e2)) = (bracket 1 e1) ++ "<=" ++ (bracket 1 e2)
      pp (BopExp(e1,GrEq,e2)) = (bracket 1 e1) ++ ">=" ++ (bracket 1 e2)
      
      precedence (Bool b) = 4
      -- *** precedence (?) = 3 -- precedence for negation Phase B only 
      precedence (Not b) = 3
      -- *** precedence (?) = 2 -- precedence for and Phase B only 
      precedence (And(b1,b2)) = 2
      -- *** precedence (?) = ? -- precedence for equality Phase B only 
      precedence (BopExp (ie,Eqq,ie')) = 1
      precedence (BopExp (ie,Le,ie')) = 1
      precedence (BopExp (ie,Gr,ie')) = 1
      precedence (BopExp (ie,LeEq,ie')) = 1
      precedence (BopExp (ie,GrEq,ie')) = 1
      
      
-- a pretty printer for errors

instance Pretty Error where 
        pp SyntaxError   = "Syntax error"
        pp  UninitializedVar  = "Error: unitialized variable"
        precedence SyntaxError   = 0
        precedence  UninitializedVar  = 0