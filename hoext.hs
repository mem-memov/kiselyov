{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Ho

class MulSYM repr where
    mul :: repr Int -> repr Int -> repr Int

instance MulSYM R where
    mul x y = R $ (unR x) * (unR y)

instance MulSYM S where
    mul x y = S $ \h -> "(" ++ (unS x) h ++ " * " ++ (unS y) h ++ ")"

tho :: (Symantics repr, MulSYM repr) => repr Int
tho = mul (int 10) th1

-- view tho
-- eval tho