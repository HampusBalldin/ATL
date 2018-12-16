module Programs where

import ATL

numE  = ValExpr . Number
nameE = NameExpr . ID
ret42 = ReturnStmt $ numE 42
addTo42 = AddExpr (numE 21) (AddExpr (numE 20) (numE 1))
printId = PrintExpr . nameE

secretNumE = SecretExpr . numE

mkSeq :: [Stmt] -> Stmt
mkSeq []     = SkipStmt
mkSeq [s]    = s
mkSeq (s:ss) = SeqStmt s (mkSeq ss)

programs = [
   program1
  ,program2
  ,program3
  ,program4
  ,program5
  ,program6
  ,program7
  ,program8
  ,program9
  ,program10
  ,program11
  ,program12
  ,program13
  ,program14
  ,program15
  ]
  
program1 = StmtProg $ ReturnStmt $ numE 42  
program2 = StmtProg $ SeqStmt SkipStmt ret42
program3 = StmtProg $ SeqStmt SkipStmt (ReturnStmt addTo42)
program4 = StmtProg $ IfThenElseStmt (numE 0)
                         (ReturnStmt (PrintExpr (numE 0)))
                         (ReturnStmt (PrintExpr (numE 1)))

program5 = StmtProg $ IfThenElseStmt (numE 1)
                (ReturnStmt (PrintExpr (numE 0)))
                (ReturnStmt (PrintExpr (numE 1)))

program6 = StmtProg $ mkSeq [
   AssignStmt (ID "x") addTo42
  ,AssignStmt (ID "y") addTo42
  ,ReturnStmt $ AddExpr (nameE "x") (nameE "y")
           ]

program6BAD = StmtProg $ mkSeq [
   AssignStmt (ID "x") (numE 1)
  ,AssignStmt (ID "x") (ValExpr NULL)
  ,ret42
           ]

program7 = StmtProg $ mkSeq [
   AssignStmt (ID "x") (numE (-10)) 
  ,WhileStmt (nameE "x") $ mkSeq [
   AssignStmt (ID "x") (AddExpr (nameE "x") (numE 1)),
   AssignStmt (ID "x") (AddExpr (nameE "x") (printId "x"))
   ]
  ,ReturnStmt $ AddExpr (nameE "x") (nameE "x")
           ]           

program8 = StmtProg $ mkSeq [
    ReturnStmt $ PrintExpr $ SecretExpr $ numE 11001010
            ]

program9 = StmtProg $ mkSeq [
   AssignStmt (ID "s") (secretNumE 11001010)
  -- ,AssignStmt (ID "s") (PrintExpr (nameE "s"))
  ,ReturnStmt $ PrintExpr (nameE "s")
            ]
           
program10 = StmtProg $ mkSeq [
    AssignStmt (ID "s") (secretNumE 11001010)
   ,AssignStmt (ID "x") (numE 42)
  -- ,AssignStmt (ID "s") (PrintExpr (nameE "s"))
  ,ReturnStmt $ PrintExpr (AddExpr (nameE "s") (nameE "x"))
            ]

program11 = StmtProg $ mkSeq [
    AssignStmt (ID "s") (secretNumE 11001010)
   ,AssignStmt (ID "x") (nameE "s") 
  -- ,AssignStmt (ID "s") (PrintExpr (nameE "s"))
  ,ReturnStmt $ PrintExpr (nameE "x")
            ]

program12     = DeclProg (ProcDecl "func" [("x", IntType), ("y", IntType)] (mkSeq [
  ReturnStmt $ AddExpr (nameE "x") (nameE "y")
               ])) program12main
program12main = StmtProg $ mkSeq [
  AssignStmt (ID "r") (CallExpr "func" (numE <$> [2, 3]))
  ,ReturnStmt $ PrintExpr (nameE "r")
    ]

program13 = DeclProg (NewTypeDecl "Person" [("age", IntType)]) program13main
program13main = StmtProg $ mkSeq [
   AssignStmt (ID "x") (NewExpr "Person")
  ,ReturnStmt (NameExpr (DeRef (ID "x") "age"))
                                 ]

program14 = DeclProg (NewTypeDecl "Person" [("age", IntType)]) program14main
program14main = StmtProg $ mkSeq [
   AssignStmt (ID "x") (NewExpr "Person")
  ,AssignStmt (DeRef (ID "x") "age") (numE 42)
  ,ReturnStmt (NameExpr (DeRef (ID "x") "age"))
                                 ]

program142 = DeclProg (NewTypeDecl "Person" [("age", IntType)]) program142main
program142main = StmtProg $ mkSeq [
   AssignStmt (ID "x") (NewExpr "Person")
  ,ReturnStmt (numE 0)
                                 ]

program15 = DeclProg (NewTypeDecl "Person" [("age", IntType)]) program15main
program15main = StmtProg $ mkSeq [
   AssignStmt (ID "x") (NewExpr "Person")
  ,AssignStmt (DeRef (ID "x") "age") (secretNumE 42)
  ,AssignStmt (ID "y") (NameExpr (DeRef (ID "x") "age"))
  ,ReturnStmt (PrintExpr (nameE "y"))
                                 ]

program16 = StmtProg $ mkSeq [
     AssignStmt (ID "x") (numE 5)
    ,AssignStmt (ID "y") (numE 5)
    ,AssignStmt (ID "z") (AddExpr (NameExpr (ID "x")) (NameExpr (ID "y")))
                             ]
