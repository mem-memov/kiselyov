{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int

newtype R a = R {unR :: a}

instance Symantics R where
    int x = R x
    add x y = R $ (unR x) + (unR y)

eval e = unR e

type VarCounter = Int
newtype S a = S {unS :: VarCounter -> String}

instance Symantics S where
    int x = S $ const $ show x
    add x y = S $ \h -> "(" ++ (unS x) h ++ " + " ++ (unS y) h ++ ")"

view e = (unS e) 0

th1 :: Symantics repr => repr Int
th1 = add (int 1) (int 2)

-- view th1
-- eval th1