module Programs where

import ATL

numE  = ValExpr . Number
nameE = NameExpr . ID
ret42 = ReturnStmt $ numE 42
addTo42 = AddExpr (numE 21) (AddExpr (numE 20) (numE 1))
printId = PrintExpr . nameE

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

program7 = StmtProg $ mkSeq [
   AssignStmt (ID "x") (numE (-10)) 
  ,WhileStmt (nameE "x") $ mkSeq [
   AssignStmt (ID "x") (AddExpr (nameE "x") (numE 1)),
   AssignStmt (ID "x") (AddExpr (nameE "x") (printId "x"))
   ]
  ,ReturnStmt $ AddExpr (nameE "x") (nameE "x")
           ]
