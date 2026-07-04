---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole 2026                                              
---------------------------------------------------------------------

module Files  where

import System.Directory

import AST
import Pretty 
import Tokens
import Parse
import IMPPar 
import EvSem 

-- Helper functions for formatting evaluation results

arrow :: String
arrow = " ==> "
 
formatValue :: Show a => [Char] -> a -> [Char]
formatValue x v = x  ++ arrow  ++ show v

formatError :: Pretty a => [Char] -> a -> [Char]
formatError x err =  x ++ "\n*** " ++ pp err
  
format :: (Show b, Pretty a) => String -> Either a b -> String
format x (Right v)  = formatValue x v 
format x (Left err) = formatError x err

-- Complete the following function that runs a single configuration 

runConf :: String -> String
runConf "" = ""
runConf x =
    case parseconf x of
        Right (COM c, s)  -> format x (evalcom (c,s))
        Right (EXP e, s)  -> format x (evalint (e,s))
        Left err          -> formatError x err 

-- The runConfs function applies the previous transformation runConf
-- to each configuration of a file's contents 

runConfs:: [String] -> [String]
runConfs = map runConf
   
-- The runAll function takes a string representing
-- the contents of a file of configurations with one configuration per line,
-- splits it into a list of configurations,
-- runs each configuraton using runConfs,
-- and then joins the runed configurations back into a single string.

runAll :: String -> String
runAll = unlines . runConfs . lines

-- Complete runFile

runFile :: FilePath -> IO()
runFile x =  do  existsFile <-  doesFileExist x 
                 if existsFile then 
                        do contents <- readFile x
                           let output = runAll contents
                           writeFile (x ++ ".output") output
                           putStrLn ("The file " ++ x ++ " was run and the output was written to " ++ x ++ ".output")
                 else putStrLn "The file does not exist" 