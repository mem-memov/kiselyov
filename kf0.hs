type Repr = Int

lit :: Int -> Repr
lit n = n

neg :: Repr -> Repr
neg e = - e

add :: Repr -> Repr -> Repr
add e1 e2 = e1 + e2