{-# LANGUAGE DeriveDataTypeable #-}
module Abstract where

import Data.Maybe
import Data.Data
import Data.Typeable

data Value
  = Void
  | IntV (Maybe Integer)
  | StrV (Maybe String)
  | BolV (Maybe Bool)
  deriving(Typeable,Data,Eq)

type Var = String

data PrimOp
  = Eq Expression Expression -- general operations

  | Add Expression Expression
  | Sub Expression Expression
  | Mult Expression Expression
  | Div Expression Expression
  | Pow Expression Expression -- number operations

  | Concat Expression Expression -- string operations

  | BOr Expression Expression
  | BAnd Expression Expression
  | BNot Expression -- boolean operations

data Expression  
  = Var Var
  | Const Value
  | PrimOp PrimOp

data Statement
  = Exp Expression
  | Declaration {n :: Var, e :: Expression}
  | Assign {n :: Var, e :: Expression}
  | While { cond :: Expression, body :: Code}
  | If {cond :: Expression, body :: Code, els :: Code}
  | Input Var
  | Print Expression
  | SepDel Integer

type Code = [Statement]
type Env = [(Var ,EnvNode)]
data EnvNode  
  = Separator Integer
  | Value Value

findReplaceFirst :: (a -> Bool) -> a -> [a] -> [a]
findReplaceFirst _ _ [] = []
findReplaceFirst p a (x:xs)
  | p x       = a:xs
  | otherwise = x:findReplaceFirst p a xs

isSameType :: Value -> Value -> Bool
isSameType l r = toConstr l == toConstr r

envChange :: Var -> Value -> Env -> Env
envChange x val env = case envLookup x env of
                        Nothing  -> error ("Variable" ++ x ++ "not declared")
                        Just old -> if isSameType val old then
                                                          findReplaceFirst (\(n,v) -> n == x) (x,Value val) env
                                                          else
                                                          error "Type missmatch" -- TODO make it less cryptic

envLookup :: Var -> Env -> Maybe Value
envLookup x env = case Prelude.lookup x env of
                    Just (Value v)  -> Just v
                    _               -> Nothing

envAdd :: Var -> Value -> Env -> Env
envAdd str node env = (str,Value node):env 

envSepAdd :: Integer -> Env -> Env
envSepAdd n env = ("",Separator n):env

envSepRem :: Integer -> Env -> Env
envSepRem _ [] = []

envSepRem n ((_, Separator m) : rest)
  | n == m = rest
  | m > n = envSepRem n rest
  | otherwise = error "Stack is in bad state LOL" -- TODO think of better way of telling user that I messed up

envSepRem n (h:t) = envSepRem n t

envEmpty :: Env
envEmpty = []

eval :: Expression -> Env -> Value 
eval (Const v) _ = v
eval (Var name) env =
  fromMaybe Void (envLookup name env)
  -- case envLookup name env of 
  --    Nothing -> NullV
  --    Just x -> x
eval (PrimOp op) env =
  case op of
    Eq e1 e2      -> let v1 = eval e1 env 
                         in let v2 = eval e2 env 
                             in BolV (Just (v1==v2))
    Add e1 e2     -> handleIntOp (+) (eval e1 env) (eval e2 env)
    Sub e1 e2     -> handleIntOp (-) (eval e1 env) (eval e2 env)
    Mult e1 e2    -> handleIntOp (*) (eval e1 env) (eval e2 env)
    Div e1 e2     -> handleIntOp div (eval e1 env) (eval e2 env)
    Pow e1 e2     -> handleIntOp (^) (eval e1 env) (eval e2 env)
    Concat e1 e2  -> handleStrOp (++) (eval e1 env) (eval e2 env)
    BNot e1       -> case eval e1 env of
                       BolV (Just a) -> BolV (Just (not a))
                       _             -> error "type error"
    BOr e1 e2     -> handleBoolOp (||) (eval e1 env) (eval e2 env)
    BAnd e1 e2    -> handleBoolOp (&&) (eval e1 env) (eval e2 env) -- PAIN PAIN PAIN PAIN


handleIntOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
handleIntOp op (IntV (Just a)) (IntV (Just b)) = IntV (Just (op a b))
handleIntOp _ _ _ = error "type error"

handleStrOp :: (String -> String -> String) -> Value -> Value -> Value
handleStrOp op (StrV (Just a)) (StrV (Just b)) = StrV (Just (op a b))
handleStrOp _ _ _ = error "type error"

handleBoolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
handleBoolOp op (BolV (Just a)) (BolV (Just b)) = BolV (Just (op a b))
handleBoolOp _ _ _ = error "type error"

interpreter :: Code -> Env -> Integer -> IO ()
interpreter [] env _ = return ()

interpreter ((Exp e):rest) env s = interpreter rest env s

interpreter ((Declaration {n = x,e = v}):rest) env s = 
  let new_env = envAdd x (eval v env) env in interpreter rest new_env s

interpreter ((Assign {n = x, e = v}):rest) env s = 
  let new_env = envChange x (eval v env) env in interpreter rest new_env s

interpreter ((Input x):rest) env s = do
  val <- getLine
  case envLookup x env of
    Nothing -> error $ "Variable " ++ x ++ "not in scope"
    Just old  -> let new_val = case old of
                                 Void   -> error "cannot read into Void-type"
                                 IntV _ -> IntV (Just (read val)) -- maybe change this later
                                 StrV _ -> StrV (Just val) -- there should be a better way to do that
                                 BolV _ -> BolV (Just (read val))
                  in interpreter (Assign {n = x, e = Const new_val}:rest) env s

interpreter ((Print e):rest) env s = do
  case eval e env of
    IntV (Just x) -> print x
    StrV (Just x) -> putStrLn x
    BolV (Just x) -> print x
    _             -> putStrLn "Void"
  interpreter rest env s

interpreter ((If {cond = c, body = b, els = e}):rest) env sepnum =
  case eval c env of
    BolV (Just c)  -> interpreter ((if c then b else e) ++ SepDel sepnum:rest) (envSepAdd sepnum env) (sepnum+1)
    BolV Nothing      -> error "condition is not defined"
    _                 -> error "condition is not of type Bool" -- maybe add case for Integer condition

interpreter ((While {cond = c, body = b}):rest) env sepnum =
  case eval c env of
    BolV (Just True) -> interpreter (b ++ SepDel sepnum:(While {cond=c, body = b}):rest) (envSepAdd sepnum env) (sepnum+1)
    _                -> interpreter rest env sepnum

interpreter ((SepDel n):rest) env sepnum
  | n >= sepnum = error "Stack is in bad state LOL" -- same as before
  | otherwise = interpreter rest (envSepRem n env) n
