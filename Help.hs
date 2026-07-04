---------------------------------------------------------------------
-- HASKELL HELP FILE FOR IMP              
-- Author: Roy L. Crole 2026                                        
---------------------------------------------------------------------

module Help where

help :: IO()
help = putStrLn (unlines [
    "",
    "Help",
    "",
    "help 1   information about IMP",
    "help 2   IMP grammar",
    "help 3   examples",
    ""
    ])

helpN :: Int -> IO()
helpN 1 = help1
helpN 2 = help2
helpN 3 = help3
helpN _ = putStrLn "Please type help 1, help 2, or help 3."

help1 :: IO()
help1 = putStrLn (unlines [
    "",
    "IMP is a small imperative language.",
    "",
    "The interpreter can evaluate:",
    "",
    "  integer expressions",
    "  boolean expressions",
    "  commands",
    "  files containing configurations",
    "",
    "A configuration has one of these forms:",
    "",
    "  (EXP <expression>, <state>)",
    "  (COM <command>, <state>)",
    "",
    "A state is a list of variable values, for example:",
    "",
    "  [(x,1),(y,2)]",
    ""
    ])

help2 :: IO()
help2 = putStrLn (unlines [
    "",
    "IMP grammar",
    "",
    "<ins>    ::= run <filename>",
    "           | eval <conf>",
    "           | quit",
    "           | help",
    "           | help <select>",
    "",
    "<select> ::= 1 | 2 | 3",
    "",
    "<conf>   ::= (<prog>, <state>)",
    "<prog>   ::= COM <com> | EXP <iexp>",
    "<state>  ::= [] | [(<idr>, <integer>), ...]",
    "",
    "<com>    ::= <idr> = <iexp>",
    "           | if <bexp> then <com> else <com>",
    "           | while <bexp> do <com>",
    "           | repeat <com> until <bexp>",
    "           | <com> ; <com>",
    "           | (<com>)",
    "",
    "<bexp>   ::= true",
    "           | false",
    "           | not <bexp>",
    "           | <bexp> & <bexp>",
    "           | <iexp> <bop> <iexp>",
    "           | (<bexp>)",
    "",
    "<bop>    ::= == | < | <= | > | >=",
    "<iop>    ::= + | - | *",
    "",
    "<iexp>   ::= <idr>",
    "           | <integer>",
    "           | <iexp> <iop> <iexp>",
    "           | (<iexp>)",
    ""
    ])

help3 :: IO()
help3 = putStrLn (unlines [
    "",
    "Examples",
    "",
    "  eval (EXP 5+7,[])",
    "  eval (EXP x+1,[(x,4)])",
    "",
    "  eval (COM x=1,[])",
    "  eval (COM x=1; y=x+2,[])",
    "  eval (COM while x<3 do x=x+1,[(x,0)])",
    "  eval (COM repeat x=x+1 until x==3,[(x,0)])",
    "",
    "  run fileA",
    "  run fileB",
    "",
    "  help",
    "  help 1",
    "  help 2",
    "  help 3",
    "",
    "  quit",
    ""
    ])