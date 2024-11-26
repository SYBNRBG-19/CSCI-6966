import qualified State as S

data LExp = Atom String | Lambda String LExp | App LExp LExp deriving (Show, Eq)

subst :: LExp -> (String,LExp) -> S.State Int LExp
subst (Atom v1) (v2, m) = if v1 == v2 then return m else return (Atom v1)
subst (App e1 e2) (v2, m) = 
    subst e1 (v2, m) >>= \e1' ->
    subst e2 (v2, m) >>= \e2' ->
    return (App e1' e2')
subst (Lambda v1 e) (v2, m) = 
    if v1 == v2
        then return (Lambda v1 e)  -- No substitution, variable is shadowed
    else
        S.get >>= \fresh ->
        let 
            newStr = "__t" ++ show fresh 
        in
            S.modify (+1) >>
            subst e (v1, Atom newStr) >>= \e' ->
            subst e' (v2, m) >>= \e'' ->
            return (Lambda newStr e'')

lexp0 = Lambda "y" (App (Atom "x") (Atom "y")) -- \y -> x y
lexp1 = Lambda "x" lexp0
lexp2 = App (Atom "y") (Atom "w") -- (y w)
lexp3 = Lambda "x" (Lambda "y" (Lambda "z" (App (App (Atom "x") (Atom "z")) (App (Atom "y") (Atom "z")))))

-- Tests:
-- > S.runState (subst lexp0 ("x",lexp2)) 0
-- (Lambda "__t0" (App (App (Atom "y") (Atom "w")) (Atom "__t0")),1)
-- > S.runState (subst lexp3 ("w", lexp2)) 0
-- (Lambda "__t0" (Lambda "__t4" (Lambda "__t6" (App (App (Atom "__t0") (Atom "__t6")) (App (Atom "__t4") (Atom "__t6"))))),7)