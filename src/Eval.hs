{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (

) where

import AST (SExpr (..))
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import LanguageError (EvalDetail (..), LangError (..))
import Location (Located (..), Position)
import Numbers (Number (..), compareNums)
import Symbols (SymbolId)

data EvalResult
    = RNumber Number
    | RBool Bool
    | RStr T.Text
    | RList [EvalResult]
    | RSymbol SymbolId
    | RBinding SymbolId EvalResult
    | RPrint EvalResult
    | RSpecialForm ([Located SExpr] -> Eval EvalResult)
    | RPrimitive ([Located EvalResult] -> Eval EvalResult)
    | RClosure [SymbolId] (Located SExpr) Env

type Env = HM.HashMap SymbolId EvalResult

newtype Eval a = Eval {runEval :: ReaderT Env (Except LangError) a}
    deriving (Monad, Applicative, Functor, MonadError LangError, MonadReader Env)

specialFormQuote :: Position -> [Located SExpr] -> Eval EvalResult
specialFormQuote pos args = case args of
    [Located (SNumber num) _] -> return (RNumber num)
    [Located (SBool boolean) _] -> return (RBool boolean)
    [Located (SSymbol sId) _] -> return (RSymbol sId)
    [Located (SList elements) _] -> do
        content <- mapM (\e -> specialFormQuote pos [e]) elements
        return (RList content)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

specialFormDefine :: Position -> [Located SExpr] -> Eval EvalResult
specialFormDefine pos args = case args of
    [Located (SSymbol sId) _, expr] -> do
        val <- eval expr
        return (RBinding sId val)
    [Located _ symPos, _] -> throwError (LEEvalError EDInvalidArg symPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

specialFormIf :: Position -> [Located SExpr] -> Eval EvalResult
specialFormIf pos args = case args of
    [cond, conseq, alt] -> do
        condVal <- eval cond
        case condVal of
            RBool False -> eval alt
            _ -> eval conseq
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

specialFormLambda :: Position -> Env -> [Located SExpr] -> Eval EvalResult
specialFormLambda pos env args = case args of
    [Located (SList argList) _, body] -> do
        idList <- getIds argList []
        return (RClosure idList body env)
    [Located _ argsPos, _] -> throwError (LEEvalError EDInvalidArg argsPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)
  where
    getIds :: [Located SExpr] -> [SymbolId] -> Eval [SymbolId]
    getIds exprs acc = case exprs of
        [] -> return (reverse acc)
        (Located (SSymbol sId) _ : xs) -> getIds xs (sId : acc)
        (Located _ idPos : _) -> throwError (LEEvalError EDInvalidArg idPos)

-- Behaves more like `let*` rather than `let` from Racket
specialFormLet :: Position -> [Located SExpr] -> Eval EvalResult
specialFormLet pos args = case args of
    [Located (SList bindings) _, body] -> do
        env <- ask
        extendedEnv <- parseBindings bindings env
        local (const extendedEnv) (eval body)
    [Located _ argsPos, _] -> throwError (LEEvalError EDInvalidArg argsPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)
  where
    parseBindings :: [Located SExpr] -> Env -> Eval Env
    parseBindings b acc = case b of
        [] -> return acc
        (Located (SList [Located (SSymbol sId) _, expr]) _ : xs) -> do
            val <- local (const acc) (eval expr)
            parseBindings xs (HM.insert sId val acc)
        (Located (SList (Located _ sPos : _)) _ : _) -> throwError (LEEvalError EDInvalidArg sPos)
        (Located (SList _) listPos : _) -> throwError (LEEvalError EDWrongArgNumber listPos)
        (Located _ listPos : _) -> throwError (LEEvalError EDInvalidArg listPos)

primCommutativeOp :: (Number -> Number -> Number) -> [Located EvalResult] -> Number -> Eval EvalResult
primCommutativeOp op args acc = case args of
    [] -> return (RNumber acc)
    (Located (RNumber num) _ : cs) ->
        primCommutativeOp op cs (num `op` acc)
    (Located _ pos : _) -> throwError (LEEvalError EDInvalidArg pos)

primArithmeticOp :: Position -> Number -> (Number -> Number -> Number) -> Bool -> [Located EvalResult] -> Eval EvalResult
primArithmeticOp pos base operation allowZero args = case args of
    [] -> throwError (LEEvalError EDWrongArgNumber pos)
    [Located val argPos] -> case val of
        RNumber num ->
            if not allowZero && isZero num
                then throwError (LEEvalError EDInvalidArg argPos)
                else return (RNumber $ base `operation` num)
        _ -> throwError (LEEvalError EDInvalidArg argPos)
    (Located firstVal firstPos : cs) -> case firstVal of
        RNumber firstNum -> do
            res <- calculate cs firstNum
            return (RNumber res)
        _ -> throwError (LEEvalError EDInvalidArg firstPos)
  where
    isZero (NInt 0) = True
    isZero (NFloat 0.0) = True
    isZero _ = False

    calculate :: [Located EvalResult] -> Number -> Eval Number
    calculate [] acc = return acc
    calculate (Located (RNumber x) numPos : xs) acc
        | allowZero && isZero x = throwError (LEEvalError EDInvalidArg numPos)
        | otherwise = calculate xs (acc `operation` x)
    calculate (Located _ argPos : _) _ = throwError (LEEvalError EDInvalidArg argPos)

primComparisonOp :: Position -> Ordering -> [Located EvalResult] -> Eval EvalResult
primComparisonOp pos ordering args = case args of
    [Located (RNumber a) _, Located (RNumber b) _] ->
        return (RBool $ compareNums a b == ordering)
    [Located _ pos1, Located _ _] -> throwError (LEEvalError EDInvalidArg pos1)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

primNot :: Position -> [Located EvalResult] -> Eval EvalResult
primNot pos args = case args of
    [Located (RBool b) _] -> return (RBool $ not b)
    [Located _ argPos] -> throwError (LEEvalError EDInvalidArg argPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

primCons :: Position -> [Located EvalResult] -> Eval EvalResult
primCons pos args = case args of
    [Located x _, Located (RList xs) _] -> return (RList (x : xs))
    [Located _ _, Located _ argPos] -> throwError (LEEvalError EDInvalidArg argPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

primCar :: Position -> [Located EvalResult] -> Eval EvalResult
primCar pos args = case args of
    [Located (RList (c : _)) _] -> return c
    [Located (RList []) argPos] -> throwError (LEEvalError EDInvalidArg argPos) -- Catch empty car
    [Located _ argPos] -> throwError (LEEvalError EDInvalidArg argPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

primCdr :: Position -> [Located EvalResult] -> Eval EvalResult
primCdr pos args = case args of
    [Located (RList (_ : cs)) _] -> return (RList cs)
    [Located (RList []) argPos] -> throwError (LEEvalError EDInvalidArg argPos) -- Catch empty cdr
    [Located _ argPos] -> throwError (LEEvalError EDInvalidArg argPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

primList :: Position -> [Located EvalResult] -> Eval EvalResult
primList _ args = return (RList [x | Located x _ <- args])

primNull :: Position -> [Located EvalResult] -> Eval EvalResult
primNull pos args = case args of
    [Located (RList content) _] -> return (RBool $ null content)
    [Located _ argPos] -> throwError (LEEvalError EDInvalidArg argPos)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

primDisplay :: Position -> [Located EvalResult] -> Eval EvalResult
primDisplay pos args = case args of
    [Located val _] -> return (RPrint val)
    _ -> throwError (LEEvalError EDWrongArgNumber pos)

eval :: Located SExpr -> Eval EvalResult
eval (Located (SNumber num) _) = return (RNumber num)
eval (Located (SBool boolean) _) = return (RBool boolean)
eval (Located (SStr str) _) = return (RStr str)
eval (Located (SSymbol sId) pos) = do
    env <- ask
    case HM.lookup sId env of
        Nothing -> throwError (LEEvalError EDUndefinedSymbol pos)
        Just val -> return val
eval (Located (SList []) _) = return (RList [])
eval (Located (SList (fnExpr : argsExpr)) pos) = do
    fnVal <- eval fnExpr
    case fnVal of
        RSpecialForm func -> func argsExpr
        RPrimitive primitive -> do
            vals <- mapM eval argsExpr
            let argsVal = zipWith (\v (Located _ exprPos) -> Located v exprPos) vals argsExpr
            primitive argsVal
        RClosure cArgs cContent cEnv -> do
            args <- mapM eval argsExpr
            nestedEnv <- bindArgs cArgs args cEnv pos
            local (const nestedEnv) (eval cContent)
        _ -> throwError (LEEvalError EDInvalidFunction pos)
  where
    bindArgs :: [SymbolId] -> [EvalResult] -> Env -> Position -> Eval Env
    bindArgs ks vs env argPos
        | length ks /= length vs = throwError (LEEvalError EDWrongArgNumber argPos)
        | otherwise = return $ foldl' (\e (k, v) -> HM.insert k v e) env (zip ks vs)
