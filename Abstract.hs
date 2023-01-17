import Data.Map(Map)
import qualified Data.Map as Map

data Type
  = Int

data Value
  = NullV
  | IntV Integer

data Expression 
  = Const Integer
  | Var { name :: String }

data Instruction
  = Exp Expression
  | ControlStatement
  | Declaration { ty :: Type, n :: String, value :: Expression}

-- date ControlStatement =
  -- | For { def :: 

type Code = [Instruction]
type Env = Map String (Type, Value)

eval :: Expression -> Env -> Value 
eval (Const n) env = IntV n
eval (Var name) env = 
  case Map.lookup name env of 
     Nothing -> NullV
     Just (t,x) -> x

interpreter :: Code -> Env -> Value
interpreter [] env = NullV
interpreter ((Exp e):rest) env = eval e env
-- TODO: skonczyn pattern matching


