module Semantics (

) where

data RevBinOp = Plus | Minus | Xor

data Op = 
    | RevOp RevBinOp
    | Mult
    | Div
    | Mod
    | FracProd
    | BitAnd
    | BitOr
    | LogAnd
    | LogOr
    | LT
    | GT
    | LE
    | GE
    | EQ 
    | NEQ

data Exp = 
    | Const Int
    | Var String
    | ArrayIndex String Exp
    | Unreversible Exp Op Exp

data Statement = 
    | SingleAssign Var RevBinOp Exp
    | ArrayAssign Var Exp RevBinOp Exp
    | IfThenElseFi Exp Statement Statement Exp
    | FromDoLoopUntil Exp Statement Statement Exp
    | Call Name
    | Uncall Name
    | Skip
    | Seq Statement Statement

data Procedure = String Statement

data Program = [Procedure]