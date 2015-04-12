module Interp where


data Term = Var Char | App Term Term | Lam Char Term deriving (Show, Eq, Ord)
-- interp. lambda term data definition

eval :: Term -> Term
eval (Var v)     = Var v
eval (Lam x t)   = Lam x (eval t)
eval (App t1 t2) = case t1 of 
                    Var v         -> Var v
                    Lam x ter     -> eval (podst t2 ter x)
                    App (Var x) y -> App t1 t2
                    App t3 t4     -> eval (App (eval t1) t2)

podst :: Term -> Term -> Char -> Term
podst ter (Var y) x 
                | x == y    = ter
                | otherwise = Var y
podst ter (Lam y tr) x 
                    | x == y    = Lam y tr
                    | otherwise = case tr of
                            Var z
                                | y == z    -> Lam y tr
                                | otherwise -> podst ter (Var z) x
                            Lam u te        -> Lam y (podst ter (Lam u te) x)
                            App tr1 tr2     -> Lam y (podst ter (App tr1 tr2) x)
podst ter (App tr1 tr2) x = App (podst ter tr1 x) (podst ter tr2 x)

id'  = Lam 'x' (Var 'x')
tru  = Lam 't' (Lam 'f' (Var 't'))
fls  = Lam 't' (Lam 'f' (Var 'f'))
test = Lam 'l' (Lam 'm' (Lam 'n' (App (App (Var 'l') (Var 'm')) (Var 'n'))))
an   = Lam 'b' (Lam 'c' (App (App (Var 'b') (Var 'c')) fls))

pair = Lam 'f' (Lam 's' (Lam 'b' (App (App (Var 'b') (Var 'f')) (Var 's'))))
fs   = Lam 'p' (App (Var 'p') tru)
sn   = Lam 'p' (App (Var 'p') fls)


id_t   = eval (App id' (App id' (Lam 'z' (App id' (Var 'z')))))
tru_t  = eval (App (App (App test tru) (Var '1')) (Var '0'))
fls_t  = eval (App (App (App test fls) (Var '1')) (Var '0'))
and_t1 = eval (App (App an tru) tru)
and_t2 = eval (App (App an tru) fls)
and_t3 = eval (App (App an fls) tru)
and_t4 = eval (App (App an fls) fls)
fs_t   = eval (App fs (App (App pair (Var '1')) (Var '2')))
sn_t   = eval (App sn (App (App pair (Var '1')) (Var '2')))


