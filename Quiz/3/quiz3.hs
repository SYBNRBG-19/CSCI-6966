fact :: [Integer]
fact = 1 : zipWith (*) fact [1..]

triangular :: [Integer]
triangular = 1 : zipWith (+) triangular [2..]

-- Question 3
{- 
    1.Abs = t1 = tx -> t2
    2.Abs = t2 = ty -> t3
    3.Abs = t3 = tz -> t4
    4.App = t5 = t6 -> t4
    5.App = tx = tz -> t5
    6.App = ty = tz -> t6
-}

-- Question 4 
{-
    t1 = (tz -> ta -> tc) -> (tz -> ta) -> tz -> tc
-}
