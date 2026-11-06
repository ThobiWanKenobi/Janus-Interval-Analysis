module Semantics (

) where

import Data.Bits hiding (Xor)

type VName = String

data RevModOp = Plus | Minus | Xor

data BinOp = 
      RevOp RevModOp
    | Mult
    | Div
    | Mod
    | FracProd
    | BitAnd
    | BitOr
    | LogicalAnd
    | LogicalOr
    | Lt
    | Gt
    | Eq
    | Neq
    | Le
    | Ge

data Val
  = ValInt Integer
  | ValArray [Integer]
  deriving (Eq, Show)

data Exp = 
      Con Integer
    | Var VName
    | Arr VName Exp
    | Bop BinOp Exp Exp

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

evalOp :: BinOp -> Integer -> Integer -> Integer
evalOp (RevOp Plus) num1 num2 = num1 + num2
evalOp (RevOp Minus) num1 num2 = num1 - num2
evalOp (RevOp Xor) num1 num2 = num1 `xor` num2
evalOp Mult num1 num2 = num1 * num2
evalOp Div num1 num2 = num1 `div` num2
evalOp Mod num1 num2 = num1 `mod` num2
evalOp FracProd num1 num2 = (num1 * num2) `div` (2 ^ 32)
evalOp BitAnd num1 num2 = num1 .&. num2
evalOp BitOr num1 num2 = num1 .|. num2
evalOp LogicalAnd num1 num2 = if num1 /= 0 && num2 /= 0 then 1 else 0
evalOp LogicalOr num1 num2 = if not $ (num1 == 0) && (num2 == 0) then 1 else 0
evalOp Lt num1 num2 = if num1 < num2 then 1 else 0
evalOp Gt num1 num2 = if num1 > num2 then 1 else 0
evalOp Eq num1 num2 = if num1 == num2 then 1 else 0
evalOp Neq num1 num2 = if num1 /= num2 then 1 else 0
evalOp Le num1 num2 = if num1 <= num2 then 1 else 0
evalOp Ge num1 num2 = if num1 >= num2 then 1 else 0

evalExp :: Env -> Exp -> Integer
evalExp env (Con val) = val
evalExp env (Var name) =
    case envLookup name env of
        Just (ValInt num) -> num
        _ -> undefined
evalExp env (Arr name e1) =
    let idx = evalExp env e1
    in
        case envLookup name env of
            Just (ValArray lst) -> lst !! fromIntegral idx
            _ -> undefined
evalExp env (Bop binOp e1 e2) =
    case (evalExp env e1, evalExp env e2) of
        (num1, num2) -> evalOp binOp num1 num2