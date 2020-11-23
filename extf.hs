{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Kf1

class MulSYM repr where
    mul :: repr -> repr -> repr

instance MulSYM Int where
    mul e1 e2 = e1 * e2

instance MulSYM String where
    mul e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"


tfm1_e = eval $ add (lit 7) (neg (mul (lit 1) (lit 2)))
tfm1_v = view $ add (lit 7) (neg (mul (lit 1) (lit 2)))

tfm2_e = mul (lit 7) tf1_e
tfm2_v = mul (lit 7) tf1_v

