import Abstract

example :: Code
example = [
            Declaration {n = "x", e = Const (IntV (Just 2137))},
            Declaration {n = "i", e = Const (IntV (Just 69))},
            While {cond = PrimOp (BNot (PrimOp(Eq (Var "i") (Const (IntV (Just 0)))))), body = 
              [
                Print(Var "x"),
                Assign {n = "x", e = PrimOp (Add (Var "x") (Const (IntV (Just 1))))},
                Assign {n = "i", e = PrimOp (Sub (Var "i") (Const (IntV (Just 1))))}
                
              ]}
          ]
