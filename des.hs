{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Kf1
import Control.Monad

data Tree = Leaf String
            | Node String [Tree]
            deriving (Read, Eq, Show)

instance ExpSYM Tree where
    lit n = Node "Lit" [(Leaf $ show n)]
    neg e = Node "Neg" [e]
    add e1 e2 = Node "Add" [e1, e2]

toTree :: Tree -> Tree
toTree = id

tf1_tree = toTree (add (lit 8) (neg (add (lit 1) (lit 2))))

type ErrMsg = String

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
                [(x, "")] -> Right x
                _ -> Left $ "Read error: " ++ s

fromTree (Node "Lit" [(Leaf n)]) = liftM lit (safeRead n)
fromTree (Node "Neg" [e]) = liftM neg (fromTree e)
fromTree (Node "Add" [e1, e2]) = liftM2 add (fromTree e1) (fromTree e2)
fromTree e = Left $ "Invalid tree: " ++ (show e)

tf1'_eval = 
    let tf1' = fromTree tf1_tree
    in case tf1' of
        Left e -> putStrLn ("Error: " ++ e)
        Right x -> print $ eval x