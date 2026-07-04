---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole 2026                                             
---------------------------------------------------------------------

module Outputs  where

import AST
import Pretty 
import Tokens
import Parse
import IMPPar 
import EvSem
import Help 
import Files

-- Helper function to handle Conf cases ; please complete 

formatConf :: Conf -> String
formatConf (COM c, s) = format "" (evalcom (c,s))
formatConf (EXP e, s) = format "" (evalint (e,s))


--------------------------------------------------------------------- -----------
-- Prompt 
----------------------------------------------------------------------------------

prompt :: IO()
prompt = do putStr "\n >IMP> \n"
            input <- getLine
            if input == "" then prompt else
             case parseins input of
              Right (Run  file)     -> runFile file >> prompt
              Right (Eval config)   -> putStr (formatConf config) >> prompt
              -- once you have completed a running version of your coursework
              -- you will need to add code here to process help instructions 
              Right Help            -> help >> prompt
              Right (HelpN n)       -> helpN n >> prompt
              Right Quit            -> return () 
              Left error            -> putStrLn (formatError "" error)  >> prompt