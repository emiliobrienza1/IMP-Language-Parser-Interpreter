--------------------------------------------------------------------
-- HASKELL PARSING LIBRARY                       
-- Roy Crole 2026
--------------------------------------------------------------------

module Parse  where

import Basic
import Tokens
import AST

-- Delete the comment from next once you completed the code for that function 
infixr 5 `next`
infixl 3 `build`
infixl 0 `alt`

  
---------------------
-- Type of parsers --
---------------------   

-- parser type: a parser of type a takes a list of tokens;
-- and returns either an error or a pair of a successful parse of type a and
-- the remaining tokens
type Parse a  = Tokens -> Either Error (a,Tokens)


-------------------
-- Basic parsers --
-------------------

-- complete the code for key
 
key :: IMPword -> Parse IMPword
key kws (Key x : toks) = if kws == x then Right (kws, toks) else Left SyntaxError
key kws _ = Left SyntaxError


-- complete the code for idr

idr :: Parse IMPword
idr (Id x : toks) = Right (x, toks)
idr toks = Left SyntaxError


-- complete the code for digits

digits :: Parse IMPword
digits (Digits x : toks) = Right (x, toks)
digits toks = Left SyntaxError



-------------------------
-- Parsing combinators --
-------------------------

alt :: Parse a -> Parse a -> Parse a
alt ph1 ph2 toks = case ph1 toks of
                     Left _ -> ph2 toks
                     Right(r,toks') -> Right(r,toks')

-- complete the code for  next

next :: Parse a -> Parse b -> Parse (a,b)
next ph1 ph2 toks = case ph1 toks of
                      Left err -> Left err
                      Right (r1, toks1) ->
                        case ph2 toks1 of
                          Left err -> Left err
                          Right (r2, toks2) -> Right ((r1, r2), toks2)
                     

-- Repetition
many :: Parse a -> Parse [a] 
many ph  = (ph `next` many ph `build` cons) `alt` (\toks -> Right([], toks))
             where
             cons (x,xs) = x:xs
 

-- The results from a parser ph are transformed by applying a function f.
build :: Parse a -> (a -> b) -> Parse b 
build ph f toks = case ph toks of
                    Left m  -> Left m
                    Right(r,toks') -> Right(f r, toks')
                    
-- appPh is a function which takes a parser ph of type a and an IMPFile
-- and parses the IMPFileto a Haskell term of type a (eg BoolExp, Com, etc).
-- Note there is an error if the input file is not fully consumed by ph 

appPh :: Parse a -> IMPFile -> Either Error a
appPh ph impf
  = case ph (tokenize impf) of
      Right (result,[]) -> Right result
      Right (result, s) -> Left SyntaxError
      Left  error   -> Left error