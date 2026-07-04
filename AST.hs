--------------------------------------------------------------------
-- HASKELL ABSTRACT SYNTAX TREES FOR IMP                  
-- Roy Crole 2026                                      
--------------------------------------------------------------------

module AST where 

type V = String
type B = Bool
type Z = Int
type FileName = String

-- Make use of the file Pretty.hs and the associated examples on Blackboard 

type State = [(V,Z)] 
type Conf  = (Program, State) 

-- *** you will need to add in the help constructors to Instruction for Phase B

data Instruction = Run FileName | Eval Conf | Quit | Help | HelpN Z
                      deriving (Eq,Show)

data Program  = COM Com | EXP IntExp
                deriving (Eq,Show)

data Com   = Ass (V, IntExp)
            | If (BoolExp, Com, Com)
            | While (BoolExp, Com)
            | Repeat (Com, BoolExp)
            | Seq (Com, Com)
            deriving (Eq,Show)
             
data BoolExp  = Bool B
            | BopExp (IntExp, Bop, IntExp)
            | Not BoolExp
            | And (BoolExp, BoolExp)
            deriving (Eq,Show)

             
data IntExp  = Int Z
            | Var V
            | IopExp (IntExp, Iop, IntExp)
            deriving (Eq,Show)                    

-- *** you will only make use of Eqq in Phase B
data Bop =   Eqq| Le | Gr | LeEq | GrEq 
             deriving (Eq,Show)

data Iop = Plus | Minus | Times 
             deriving (Eq,Show)
              
type IntExpConf = (IntExp, State)
type BoolExpConf = (BoolExp, State)
type ComConf = (Com, State)