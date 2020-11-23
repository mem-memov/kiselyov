{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Kf1 where

class ExpSYM repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

instance ExpSYM Int where
    lit n = n
    neg e = - e
    add e1 e2 = e1 + e2

instance ExpSYM String where
    lit n = show n
    neg e = "(-" ++ e ++ ")"
    add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++")"

eval :: Int -> Int
eval = id

view :: String -> String
view = id

tf1_e = eval (add (lit 8) (neg (add (lit 1) (lit 2))))
tf1_v = view (add (lit 8) (neg (add (lit 1) (lit 2))))

