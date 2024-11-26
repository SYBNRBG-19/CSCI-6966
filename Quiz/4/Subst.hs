data LExp = Atom String | Lambda String LExp | App LExp LExp deriving (Show, Eq)

-- Substition function gets fresh index as argument and returns new fresh index
subst :: LExp -> (String,LExp) -> Int -> (LExp, Int)
-- (Atom v1)[m/v2]] -- substitutes v2 for m in variable expression Atom v1
subst (Atom v1) (v2,m) fresh =  
    if v1 == v2 
        then (m,fresh) 
        else (Atom v1,fresh)

-- (App e1 e2)[m/v2] -- substitutes m for v2 in application expression App e1 e2
subst (App e1 e2) (v2,m) fresh = 
    let (e1',fresh') = subst e1 (v2,m) fresh
        (e2',fresh'') = subst e2 (v2,m) fresh' in
            (App e1' e2',fresh'')

-- (Lambda v1 e)[m/v2] -- substitutes m for v2 in application expression Lambda v1 e
subst (Lambda v1 e) (v2,m) fresh = 
    if v1 == v2
        then (Lambda v1 e, fresh) -- no substitution, shadowing
        else let newStr = "__t" ++ (show fresh) -- generate a fresh name 
                 -- substitute (Atom newStr) for v1 in e; pass new fresh index
                 (e',fresh') = subst e (v1, Atom newStr) (fresh + 1)
                 -- substitute m for v2 in e'. No clashes as all bound vars are fresh
                 (e'',fresh'') = subst e' (v2,m) fresh' in
                    (Lambda newStr e'', fresh'') -- return the new expression

lexp0 = Lambda "y" (App (Atom "x") (Atom "y")) -- \y -> x y
lexp1 = Lambda "x" lexp0
lexp2 = App (Atom "y") (Atom "w") -- (y w)
lexp3 = Lambda "x" (Lambda "y" (Lambda "z" (App (App (Atom "x") (Atom "z")) (App (Atom "y") (Atom "z")))))

-- Tests:
-- > subst lexp0 ("x", lexp2) 0
-- (Lambda "__t0" (App (App (Atom "y") (Atom "w")) (Atom "__t0")),1)
-- > subst lexp3 ("w", lexp2) 0
-- (Lambda "__t0" (Lambda "__t4" (Lambda "__t6" (App (App (Atom "__t0") (Atom "__t6")) (App (Atom "__t4") (Atom "__t6"))))),7)