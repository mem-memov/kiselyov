{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

tf1_Int :: Int
tf1_Int = add (lit 8) (neg (add (lit 1) (lit 2)))

tf1_String :: String
tf1_String = add (lit 8) (neg (add (lit 1) (lit 2)))