  
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole 2026                                            
---------------------------------------------------------------------

module Main where

import AST
import Basic
import Pretty 
import Tokens
import Parse
import IMPPar 
import EvSem
import Files
import Help 
import Outputs

--------------------------
--- Welcoming message -----
---------------------------

introduction = (putStr . unlines) [

 "\n ",
 "                           I         M              M         PPPP                   ",
 "                           I         MM           M M         P    P                 ",
 "                           I         M  M       M   M         P     P                ",
 "                           I         M    M   M     M         P     P                ",
 "                           I         M      M       M         P    P                 ",
 " Welcome to the world of   I         M              M         PPPP                   ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",

 "\n Copyright (c) Leicester University, 2026 ",

 "\n Please type help for help    ...    recursion is everywhere  ... " ]

------------------------
--  main 
-------------------------

main :: IO()
main = do  introduction
           prompt 
           
          
