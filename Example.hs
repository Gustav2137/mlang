import Abstract

helloWorld :: FunEnv
helloWorld = [("main",[],[Print (Const (StrV (Just "Hello World!")))])]

echo :: FunEnv
echo = [("main",[],[
            Declaration {n = "x", e = Const (StrV (Just ""))},
            While {cond = PrimOp (BNot (PrimOp(Eq (Var "x") (Const (StrV (Just "q")))))), body = 
              [
                Print(Var "x"),
                Input "x"
              ]}
       ])]

ex :: FunEnv
ex = [("main",[],[
        Declaration "x" (Const (IntV Nothing)),
        Input "x",
        While {cond = PrimOp (BNot (PrimOp (Eq (Var "x") (Const (IntV (Just 0)))))), body = 
          [
            Print(Var "x"),
            Assign "x" (PrimOp (Sub (Var "x") (Const (IntV (Just 1)))))
          ]}
      ])]

add :: FunEnv
add = [("add",[("x",IntV Nothing),("y",IntV Nothing)],[
        Return $ PrimOp (Add (Var "x") (Var "y")),
        Print (Const (StrV $ Just "XDXDXD"))
        ]),
        ("main",[],[
          Declaration "x" (Const (IntV $ Just 10)),
          Declaration "y" (Const (IntV $ Just 3)),
          Print (Apply "add" [Var "x",Var "y"])
        ])]

factorialRec :: FunEnv
factorialRec = [("fact",[("n",IntV Nothing)],[
          If (PrimOp (BOr (PrimOp (Eq (Var "n") (Const (IntV $ Just 0)))) (PrimOp (Eq (Var "n") (Const (IntV $ Just 1)))))) 
            [Return $ Var "n"]
            [Return $ PrimOp (Mult (Var "n") (Apply "fact" [PrimOp $ Sub (Var "n") (Const (IntV $ Just 1))]))]]),
               ("main",[],[
                  Declaration "x" (Const $ IntV Nothing),
                  Input "x",
                  Print $ Apply "fact" [Var "x"]])]

rec :: FunEnv 
rec = [("rec",[("x",IntV Nothing)],[
                  Print $ Var "x",
                  Return $ Apply "rec" [PrimOp $ Add (Var "x") (Const (IntV $ Just 1))]]),
                 ("main",[],[
                  Exp $ Apply "rec" [Const (IntV $ Just 1)]])]
                  
