{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Abstract where

import Data.Maybe
import Data.Data
import Data.Typeable
import Control.Monad

data Value
  = Void
  | IntV (Maybe Integer)
  | StrV (Maybe String)
  | BolV (Maybe Bool)
  deriving(Typeable,Data,Eq,Show)

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
  | Apply Var [Expression]

data Statement
  = Exp Expression
  | Declaration {n :: Var, e :: Expression}
  | Assign {n :: Var, e :: Expression}
  | While { cond :: Expression, body :: Code}
  | If {cond :: Expression, body :: Code, els :: Code}
  | Input Var
  | Print Expression
  | SepDel Integer
  | Return Expression

type Code = [Statement]
type FunDef = (Var, [(Var,Value)], Code)
type FunEnv = [FunDef]
type Env = [(Var, EnvNode)]
data EnvNode  
  = Separator Integer
  | Value Value

eqtype :: [Value] -> [Value] -> Bool
eqtype (x:xs) (y:ys) = 
  isSameType x y && eqtype xs ys

eqtype [] [] = True
eqtype _ [] = False
eqtype [] _ = False

findFunction :: Var -> [Value] -> FunEnv -> FunDef
findFunction f args = 
  head . filter (\(x,a,_) -> x == f && eqtype (map snd a) args)
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

eval :: Expression -> Env -> FunEnv -> IO Value
eval (Const v) _  _ = return v
eval (Var name) env _ =
  return $ fromMaybe Void (envLookup name env)
  -- case envLookup name env of 
  --    Nothing -> NullV
  --    Just x -> x
eval (PrimOp op) env fenv =
  let helper handler op e1 e2 env fenv = do
        x <- eval e1 env fenv
        y <- eval e2 env fenv
        return $ handler op x y in
  case op of
    Eq e1 e2      -> do 
      v1 <- eval e1 env fenv
      v2 <- eval e2 env fenv
      return $ BolV (Just (v1==v2))
    Add e1 e2     -> helper handleIntOp (+) e1 e2 env fenv 
    Sub e1 e2     -> helper handleIntOp (-) e1 e2 env fenv
    Mult e1 e2    -> helper handleIntOp (*) e1 e2 env fenv
    Div e1 e2     -> helper handleIntOp div e1 e2 env fenv
    Pow e1 e2     -> helper handleIntOp (^) e1 e2 env fenv
    Concat e1 e2  -> helper handleStrOp (++) e1 e2 env fenv
    BNot e1       -> do
      x <- eval e1 env fenv
      case x of
        BolV (Just a) -> return $ BolV (Just (not a))
        _             -> error "type error"
    BOr e1 e2     -> helper handleBoolOp (||) e1 e2 env fenv
    BAnd e1 e2    -> helper handleBoolOp (&&) e1 e2 env fenv -- PAIN PAIN PAIN PAIN
       

eval (Apply f args) env fenv = do
  args <- mapM (\x -> eval x env fenv) args
  let (_, params, body) = findFunction f args fenv in 
    interpreter body (mkEnv params args) fenv 0

mkEnv :: [(Var, Value)] -> [Value] -> Env
mkEnv ((x,_):xs) (y:ys) = (x, Value y): mkEnv xs ys
mkEnv [] [] = []
mkEnv (x:xs) [] = error "shouldnt happen"
mkEnv [] (y:ys) = error "shouldnt happen"

handleIntOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
handleIntOp op (IntV (Just a)) (IntV (Just b)) = IntV (Just (op a b))
handleIntOp _ _ _ = error "type error"

handleStrOp :: (String -> String -> String) -> Value -> Value -> Value
handleStrOp op (StrV (Just a)) (StrV (Just b)) = StrV (Just (op a b))
handleStrOp _ _ _ = error "type error"

handleBoolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
handleBoolOp op (BolV (Just a)) (BolV (Just b)) = BolV (Just (op a b))
handleBoolOp _ _ _ = error "type error"

interpreter :: Code -> Env -> FunEnv -> Integer -> IO Value
interpreter [] env fenv _ = return Void

interpreter ((Return x):rest) env fenv s = do
  eval x env fenv
  
interpreter ((Exp e):rest) env fenv s = eval e env fenv >> interpreter rest env fenv s

interpreter ((Declaration {n = x,e = e}):rest) env fenv s = do
  v <- eval e env fenv 
  let new_env = envAdd x v env in interpreter rest new_env fenv s

interpreter ((Assign {n = x, e = e}):rest) env fenv s = do
  v <- eval e env fenv
  let new_env = envChange x v env in interpreter rest new_env fenv s

interpreter ((Input x):rest) env fenv s = 
  getLine >>=
  (\val -> case envLookup x env of
  Nothing -> error $ "Variable " ++ x ++ "not in scope"
  Just old  -> let new_val = case old of
                                 Void   -> error "cannot read into Void-type"
                                 IntV _ -> IntV (Just (read val)) -- maybe change this later
                                 StrV _ -> StrV (Just val) -- there should be a better way to do that
                                 BolV _ -> BolV (Just (read val))
                  in interpreter (Assign {n = x, e = Const new_val}:rest) env fenv s)

interpreter ((Print e):rest) env fenv s = do
  eval e env fenv >>= \case
    IntV (Just x) -> print x
    StrV (Just x) -> putStrLn x
    BolV (Just x) -> print x
    _             -> putStrLn "Void"
  interpreter rest env fenv s

interpreter ((If {cond = c, body = b, els = e}):rest) env fenv sepnum =
  eval c env fenv >>= \case 
    BolV (Just c)  -> interpreter ((if c then b else e) ++ SepDel sepnum:rest) (envSepAdd sepnum env) fenv (sepnum+1)
    BolV Nothing      -> error "condition is not defined"
    _                 -> error "condition is not of type Bool" -- maybe add case for Integer condition

interpreter ((While {cond = c, body = b}):rest) env fenv sepnum =
  eval c env fenv >>= \case 
    BolV (Just True) -> interpreter (b ++ SepDel sepnum:(While {cond=c, body = b}):rest) (envSepAdd sepnum env) fenv (sepnum+1)
    _                -> interpreter rest env fenv sepnum

interpreter ((SepDel n):rest) env fenv sepnum
  | n >= sepnum = error "Stack is in bad state LOL" -- same as before
  | otherwise = interpreter rest (envSepRem n env) fenv n

run :: FunEnv -> IO ()
-- TODO add support for commandline arguments
run fenv = void $ interpreter (fromJust $ Prelude.lookup "main" $ map (\(x,_,y) -> (x,y)) fenv) envEmpty fenv 0
