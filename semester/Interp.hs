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


id'  = Lam 'x' (Var 'x')                                                       -- id   = λx. x
tru  = Lam 't' (Lam 'f' (Var 't'))                                             -- tru  = λt. λf. t
fls  = Lam 't' (Lam 'f' (Var 'f'))                                             -- fls  = λt. λf. f
test = Lam 'l' (Lam 'm' (Lam 'n' (App (App (Var 'l') (Var 'm')) (Var 'n'))))   -- test = λl. λm. λn. l m n
an   = Lam 'b' (Lam 'c' (App (App (Var 'b') (Var 'c')) fls))                   -- and  = λb. λc. b c fls

pair = Lam 'f' (Lam 's' (Lam 'b' (App (App (Var 'b') (Var 'f')) (Var 's'))))   -- pair = λf.λs.λb. b f s
fs   = Lam 'p' (App (Var 'p') tru)                                             -- fst  = λp. p tru
sn   = Lam 'p' (App (Var 'p') fls)                                             -- snd  = λp. p fls


id_t   = eval (App id' (App id' (Lam 'z' (App id' (Var 'z')))))                -- id (id (λz. id z))
tru_t  = eval (App (App (App test tru) (Var '1')) (Var '0'))                   -- test tru v w
fls_t  = eval (App (App (App test fls) (Var '1')) (Var '0'))                   -- test fls v w
and_t1 = eval (App (App an tru) tru)                                           -- and tru tru
and_t2 = eval (App (App an tru) fls)                                           -- and tru fls
and_t3 = eval (App (App an fls) tru)                                           -- and fls tru
and_t4 = eval (App (App an fls) fls)                                           -- and fls fls
fs_t   = eval (App fs (App (App pair (Var '1')) (Var '2')))                    -- fst (pair v w)
sn_t   = eval (App sn (App (App pair (Var '1')) (Var '2')))                    -- snd (pair v w)


