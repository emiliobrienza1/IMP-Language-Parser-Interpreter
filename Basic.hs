------------------------------------------------------------------- 
-- HASKELL BASIC LIST AND STRING PROCESSING FUNCTIONS           
-- Roy L. Crole 2026                                     
------------------------------------------------------------------- 

module Basic where

data Error =  SyntaxError | UninitializedVar
              deriving (Eq, Show)

-- code mem
mem :: Eq a => a -> [a] -> Bool
mem x []     = False
mem x (h:t)  = if x == h then True else mem x t

-- code lookUp
lookUp :: Eq a => [(a,b)] -> a -> Either Error b
lookUp [] v             = Left UninitializedVar
lookUp ((x,y):pairs) v  = if x == v then Right y else lookUp pairs v


-- code update
update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
update [] v z = [(v,z)]
update ((v,z):pairs) v' z'  = if v == v' then ((v',z'):pairs) else ((v,z):(update pairs v' z'))

