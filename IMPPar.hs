--------------------------------------------------------------------
-- HASKELL IMP LANGUAGE PARSING LIBRARY                   
-- Roy Crole 2026                                         
--------------------------------------------------------------------

module IMPPar where

import Basic
import AST
import Tokens
import Parse
import Pretty


----------------------------------
-- Bookkeeping Functions        --
----------------------------------

str_to_z :: String -> Z
str_to_z = read

str_to_v :: String -> V 
str_to_v s = s

str_to_b :: String -> B
str_to_b s = if s == "true" then True else False


----------------------------------
-- Top Level Parsing Functions  --
----------------------------------

parseins :: IMPFile -> Either Error Instruction
parseins = appPh ins 

parsestate :: IMPFile -> Either Error State 
parsestate = appPh state

parseconf :: IMPFile -> Either Error Conf
parseconf = appPh conf 

parsecom :: IMPFile ->  Either Error Com
parsecom  = appPh com

parseiexp :: IMPFile -> Either Error IntExp 
parseiexp = appPh iexp

parsebexp :: IMPFile -> Either Error BoolExp 
parsebexp = appPh bexp


---------------------
-- Build Functions --
---------------------

-- parser build functions for the State
makeEmptystate s = []
makeState  ("[",("(",(v,(",",(z,(")",(s,"]"))))))) =
                (v, str_to_z z) : (map aux s)
                where
                aux (",",("(",(v,(",",(z,")"))))) = (v,str_to_z z)

-- parser build functions for configurations
makeConf ("(",(program,(",",(s,")")))) = case program of
                                           COM c -> (COM c, s)
                                           EXP e -> (EXP e, s)

-- parser build functions for programs
makeCProgram ("COM",c) = COM c
makeEProgram ("EXP",e) = EXP e

-- parser build functions for commands and expressions
makeExpFromAtom  (_,(e,_)) = e
makeVar = Var . str_to_v
makeInt = Int . str_to_z
makeNegInt ("-",ie) = Int (-(str_to_z ie))

makePMT (a, la) = let mPM e1 (op,e2) = 
                       case op of
                          "+" -> IopExp(e1,Plus,e2)
                          "-" -> IopExp(e1,Minus,e2)
                          "*" -> IopExp(e1,Times,e2)
                      in foldl mPM a la

-- *** once your interpreter is working, that is, you can successfully run Main.hs for Phase A of IMP,
-- *** add in lines of code for make functions for logical negation and logical and for Phase B if you wish 

makeBool (ie1,(op,ie2)) = case op of
                            "==" -> BopExp (ie1,Eqq,ie2)
                            "<"  -> BopExp (ie1,Le,ie2)
                            ">"  -> BopExp (ie1,Gr,ie2)
                            "<=" -> BopExp (ie1,LeEq,ie2)
                            ">=" -> BopExp (ie1,GrEq,ie2)

makeNot ("not",b) = Not b

makeAnd (b,lb) = foldl aux b lb
                 where
                 aux b1 ("&",b2) = And (b1,b2)

makeComFromAtom  ("(",(c,")")) = c
makeAss (v,("=",e)) = Ass (v,e)

makeSeq (c,lc) = foldl aux c lc
                 where
                 aux c1 (";",c2) = Seq (c1,c2)

makeIfte ("if",(be,("then",(c1,("else",c2))))) = If (be,c1,c2)
makeWhile ("while",(be,("do",c))) = While (be,c) 

-- *** once your interpreter is working, that is, you can successfully run Main.hs for Phase A 
-- *** add in lines of code for make functions for repeat loops 

makeRepeat ("repeat",(c,("until",be))) = Repeat (c,be)


----------------------------
-- The Combinatory Parser --
----------------------------

-- parser for IMP integers
-- NOTE the type
integer :: Parse IMPFile
integer toks = ( digits
                `alt`
                 key"-" `next` digits `build` (\("-",d) -> "-" ++ d)
               ) toks

-- parser for an IMP instruction
ins :: Parse Instruction
ins = key "run" `next` idr  `build` (\("run",p)-> Run p)
          `alt`
           key"eval" `next` conf `build` (\("eval",p)-> Eval p)
           `alt` 
           -- *** once your interpreter is fully working, that is, you can successfully run Main.hs for Phase A 
           -- *** add in lines of code for for help instructions for Phase B 
           key"help" `next` integer `build` (\("help",n)-> HelpN (str_to_z n))
           `alt`
           key"help" `build` (\i -> Help)
           `alt`
           key"quit" `build` (\i -> Quit)
           
-- parser for an IMP state
state :: Parse State 
state = key"[]" `build` makeEmptystate
        `alt`
        key"[" `next` 
        key"(" `next` idr `next` key","
        `next`
        integer 
        `next` key")"
        `next`
        many (key"," `next` 
              key"(" `next` idr `next` key"," `next` integer  `next` key")")
        `next` key"]"
        `build` makeState

-- parser for IMP configurations
conf :: Parse Conf 
conf toks = (key"(" `next` program `next` key"," `next` state `next` key")" `build` makeConf) toks

-- parser for IMP program: it needs completing! 
program :: Parse Program
program toks = (key"COM" `next` com `build` makeCProgram
               `alt`
                key"EXP" `next` iexp `build` makeEProgram) toks


-- parser for IMP commands
-- complete the parser 
com :: Parse Com
com toks = 
  (iwr `next` many (key";" `next` iwr) `build` makeSeq 
  ) toks
  
iwr :: Parse Com
iwr toks =
  (key"if" `next` bexp `next` key"then" `next` catom `next` key"else" `next` catom `build` makeIfte
  `alt`
   key"while" `next` bexp `next` key"do" `next` catom `build` makeWhile
-- *** once your interpreter is working, that is, you can successfully run Main.hs for Phase A
-- *** add in lines of code for parsing repeat loops 
  `alt`
   key"repeat" `next` catom `next` key"until" `next` bexp `build` makeRepeat
  `alt`
   catom 
  ) toks
  
catom  :: Parse Com
catom toks = 
  (idr `next` key"=" `next` iexp `build` makeAss 
  `alt` 
   key"(" `next` com `next` key")" `build` makeComFromAtom
  ) toks

      
-- parser for IMP Boolean expressions
-- there are just a couple of things to complete for parser batom
bexp :: Parse BoolExp
-- *** once your interpreter is working, that is, you can successfully run Main.hs for Phase A
-- *** add in lines of code for parsing logical negation along with logical and 
-- *** along with equality on integers 
-- *** this will also require editing the code below !!!! ***
bexp toks =
   (batom `next` many (key"&" `next` batom) `build` makeAnd
  ) toks

batom :: Parse BoolExp
batom toks = 
  (key"true" `build` (Bool . str_to_b)
   `alt`
   key"false" `build` (Bool . str_to_b) 
   `alt`
   key"not" `next` batom `build` makeNot
   `alt`
   iexp
   `next`
   (key"==" `alt` key"<" `alt` key">" `alt` key"<=" `alt` key">=")
   `next`
   iexp
   `build` makeBool
   `alt`
   key"(" `next` bexp `next` key")"  `build` makeExpFromAtom
  )  toks


-- parser for IMP integer expressions
-- complete
iexp :: Parse IntExp
iexp toks = 
  (factor `next` many ((key"+" `alt` key"-") `next` factor) `build` makePMT
  ) toks

factor :: Parse IntExp
factor toks = 
  (iatom `next` many (key"*" `next` iatom) `build` makePMT
  ) toks

iatom :: Parse IntExp
iatom toks = 
  (idr `build` makeVar 
  `alt`
   digits `build` makeInt -- deal with "positive integers" 
   `alt`
   key"-" `next` digits `build` makeNegInt -- deal with "negative integers" 
  `alt` 
   key"(" `next` iexp `next` key")" `build` makeExpFromAtom -- hint: cross reference batom and only uncomment once you have coded iexp! 
  ) toks 