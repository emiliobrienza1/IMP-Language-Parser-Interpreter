---------------------------------------------------------------------------
-- HASKELL TOKENS FOR EXPRESSIONS AND COMMANDS FOR IMPERATIVE LANGUAGE IMP 
-- Roy L. Crole 2026                                           
---------------------------------------------------------------------------

module Tokens where

import Basic


type IMPFile = String
type IMPword = String
type IMPwords = [IMPword]

-- a token is an identifier, keyword or natural number 

data Token = Id IMPword | Key IMPword | Digits IMPword
             deriving (Show, Eq)
             
type Tokens = [Token]

--- Keywords: *** you will need to include keywords for logical negation, along with repeat loops to complete Phase B

keywords  :: IMPwords
keywords = ["true","false","while","do",
            "if","then","else",
            "not","repeat","until",
            "run","eval","quit","help","COM","EXP"]

-- Symbols: *** you will need to add a symbol other than = for equality testing, along with logical and, for Phase B 
symbols :: IMPwords
symbols = ["(",")","+","-","*","<=",">=","<",">",";","=","==","&","[",",","]","[]"]

-- complete the code for is_letter
is_letter :: Char -> Bool
is_letter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

-- complete the code for is_digit
is_digit :: Char -> Bool
is_digit c = '0' <= c && c <= '9'

specials = ",!@#$%^&*()_-+=|[]:;'~`<>.?/"

-- delete the comment from is_special if you have already coded mem in Basics.hs
is_special :: Char -> Bool
is_special c = c `mem` specials

-- complete the code for the function alpha
alpha :: (String, IMPword) -> (String, IMPword)
alpha (al, c:cs) = if is_letter c then alpha(al++[c],cs) else (al, c:cs)
alpha (al,[]) = (al,[])

-- complete the code for the function numeric
numeric :: (String, IMPword) -> (String, IMPword) 
numeric (nu, c:cs) = if is_digit c then numeric(nu++[c],cs) else (nu, c:cs)
numeric (nu,[]) = (nu,[])

-- complete the code for symbolic
symbolic :: (String, IMPword) -> (String, IMPword)  
symbolic (sy, c:cs) = 
  if is_special c then
    if
      (sy++[c]) `mem` symbols  
    then
      symbolic (sy++[c],cs)
    else
      (sy, c:cs)
  else
    (sy, c:cs)
symbolic (sy, []) = (sy, [])

----------------------------------
-- complete the code for scanning
-----------------------------------

scanning :: (Tokens, IMPFile) -> Tokens 
scanning (toks, []) = toks
scanning (toks, c:cs) = 
      if 
        is_letter c
      then
        let (al, cs2) = alpha([c],cs) in 
        if al `mem` keywords then scanning (toks++[Key al],cs2) else scanning (toks++[Id al],cs2)
      else 
        if is_digit c
        then
          let (nu, cs2) = numeric([c],cs) in
          scanning (toks++[Digits nu],cs2)
        else 
          if
            is_special c
          then
            let (sy, cs2) = symbolic([c],cs) in
            if sy `mem` symbols then scanning (toks++[Key sy],cs2) else scanning (toks,cs2)
          else
            scanning (toks,cs)

-- Delete the comments for tokenize once you code scanning
tokenize :: IMPFile -> Tokens 
tokenize impf = scanning([], impf)