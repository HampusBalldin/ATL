module ATL where

type Identifier     = String
type TypeBinding    = [(Identifier, Type)]

data Name = ID Identifier 
          | DeRef Name Identifier
          deriving (Eq, Show)

data Val = Number { integer :: Integer }
         | NULL
         deriving (Eq, Show)

data Expr = ValExpr Val
          | AddExpr Expr Expr
          | NameExpr Name
          | PrintExpr Expr
          | NewExpr Identifier
          | CallExpr Identifier [Expr]
          | SecretExpr Expr
          deriving (Eq, Show)

data Type = IntType
          | IdType Identifier
          deriving (Eq, Show)

data Stmt = AssignStmt Name Expr
          | BlockStmt Stmt
          | IfThenElseStmt Expr Stmt Stmt
          | WhileStmt Expr Stmt
          | SkipStmt
          | ReturnStmt Expr
          | SeqStmt Stmt Stmt
          deriving (Eq, Show)

data Decl = ProcDecl Identifier TypeBinding Stmt
          | NewTypeDecl Identifier TypeBinding
          deriving (Eq, Show)

data Prog = DeclProg Decl Prog
          | StmtProg Stmt
          deriving (Eq, Show)
