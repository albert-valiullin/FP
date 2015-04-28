module TypeLambda where

data Ty = TyBool | TyArr Ty Ty deriving (Show, Eq, Ord)
-- interp. type definition with boolean base type

type Context = [(Char, Ty)]
-- interp. context for variable and type binding

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmVar Char
          | TmLam Char Ty Term
          | TmApp Term Term
          deriving (Show, Eq, Ord)
-- interp. lambda term data definition (with boolean types)

getType :: Context -> Char -> Either Bool Ty
getType [] x = Left False
getType ((v,ty):ctx) x | v == x    = Right ty
                       | otherwise = getType ctx x

typeof :: Context -> Term -> Either Bool Ty
typeof ctx term = case term of
                    TmTrue          -> Right TyBool
                    TmFalse         -> Right TyBool
                    TmIf t1 t2 t3   -> case [typeof ctx t1, typeof ctx t2, typeof ctx t3] of
                                        [Right TyBool, Right tyT2, Right tyT3] | tyT3 == tyT2 -> Right tyT2
                                                                               | otherwise    -> Left False
                                        _ -> Left False
                    TmVar v         -> getType ctx v
                    TmLam x tyT1 t2 -> case typeof ((x,tyT1):ctx) t2 of
                                         Right tyT2 -> Right (TyArr tyT1 tyT2)
                                         _ -> Left False
                    TmApp t1 t2     -> case (typeof ctx t1, typeof ctx t2) of
                                         (Right (TyArr tyT11 tyT12), Right tyT2) | tyT2 == tyT11 -> Right tyT12
                                                                                 | otherwise     -> Left False
                                         _ -> Left False

checkType :: Term -> Bool
checkType term = case typeof [] term of
                    Left _  -> False
                    Right _ -> True

if_t1 = (TmIf TmFalse TmTrue TmFalse)                                                                -- if false true false
if_t2 = (TmApp (TmLam 'f' TyBool (TmVar 'f')) (TmIf TmFalse TmTrue TmFalse))                         -- (λf::Bool. f) (if false true false)
if_f1 = (TmApp (TmLam 'f' (TyArr TyBool TyBool) (TmVar 'f')) (TmIf TmFalse TmTrue TmFalse))          -- (λf::(Bool -> Bool). f) (if false true false)
if_f2 = (TmApp (TmLam 'f' TyBool (TmVar 'f')) (TmIf TmFalse TmTrue (TmLam 't' TyBool (TmVar 't'))))  -- (λf::Bool. f) (if false true (λf::Bool. f))