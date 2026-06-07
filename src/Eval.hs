{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (

) where

import AST (SExpr (..))
import Control.Monad (when)
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
    | RSExpr (Located SExpr)
    | RBinding SymbolId EvalResult
    | RSpecialForm ([Located SExpr] -> Eval EvalResult)
    | RPrimitive ([Located EvalResult] -> Eval EvalResult)
    | RClosure [SymbolId] (Located SExpr) Env

type Env = HM.HashMap SymbolId EvalResult

newtype Eval a = Eval {runEval :: ReaderT Env (Except LangError) a}
    deriving (Monad, Applicative, Functor, MonadError LangError, MonadReader Env)

specialFormDefine :: Position -> [Located SExpr] -> Eval EvalResult
specialFormDefine _ [Located (SSymbol sId) _, expr] = do
    val <- eval expr
    return (RBinding sId val)
specialFormDefine _ (Located _ symPos : _) = throwError (LEEvalError EDInvalidArg symPos)
specialFormDefine pos _ = throwError (LEEvalError EDWrongArgNumber pos)

specialFormIf :: Position -> [Located SExpr] -> Eval EvalResult
specialFormIf _ [cond, conseq, alt] = do
    condVal <- eval cond
    case condVal of
        RBool False -> eval alt
        _ -> eval conseq
specialFormIf pos _ = throwError (LEEvalError EDWrongArgNumber pos)

specialFormLambda :: Position -> Env -> [Located SExpr] -> Eval EvalResult
specialFormLambda _ env [Located (SList argList) _, body] = do
    idList <- getIds argList []
    return (RClosure idList body env)
  where
    getIds :: [Located SExpr] -> [SymbolId] -> Eval [SymbolId]
    getIds [] acc = return (reverse acc)
    getIds (Located (SSymbol sId) _ : xs) acc = getIds xs (sId : acc)
    getIds (Located _ pos : _) _ = throwError (LEEvalError EDInvalidArg pos)
specialFormLambda _ _ (Located _ argsPos : _) = throwError (LEEvalError EDInvalidArg argsPos)
specialFormLambda pos _ _ = throwError (LEEvalError EDWrongArgNumber pos)

specialFormLet :: Position -> [Located SExpr] -> Eval EvalResult
specialFormLet _ [Located (SList bindings) _, body] = do
    bindVal <- parseBindings bindings []
    env <- ask
    let extendedEnv = foldl' (\e (k, v) -> HM.insert k v e) env bindVal
    local (const extendedEnv) (eval body)
  where
    parseBindings :: [Located SExpr] -> [(SymbolId, EvalResult)] -> Eval [(SymbolId, EvalResult)]
    parseBindings [] acc = return (reverse acc)
    parseBindings (Located (SList [Located (SSymbol sId) _, expr]) _ : xs) acc = do
        val <- eval expr
        parseBindings xs ((sId, val) : acc)
    parseBindings (Located (SList (Located _ sPos : _)) _ : _) _ = throwError (LEEvalError EDInvalidArg sPos)
    parseBindings (Located (SList _) listPos : _) _ = throwError (LEEvalError EDWrongArgNumber listPos)
    parseBindings (Located _ listPos : _) _ = throwError (LEEvalError EDInvalidArg listPos)
specialFormLet _ (Located _ argsPos : _) = throwError (LEEvalError EDInvalidArg argsPos)
specialFormLet pos _ = throwError (LEEvalError EDWrongArgNumber pos)

primArithmeticOp :: Position -> (Number -> Number -> Number) -> [Located EvalResult] -> Eval EvalResult
primArithmeticOp _ operation [Located (RNumber a) _, Located (RNumber b) _] =
    return (RNumber $ a `operation` b)
primArithmeticOp _ _ (Located _ pos : _) = throwError (LEEvalError EDInvalidArg pos)
primArithmeticOp pos _ _ = throwError (LEEvalError EDWrongArgNumber pos)

primComparisonOp :: Position -> Ordering -> [Located EvalResult] -> Eval EvalResult
primComparisonOp _ ordering [Located (RNumber a) _, Located (RNumber b) _] =
    return (RBool $ compareNums a b == ordering)
primComparisonOp _ _ (Located _ pos : _) = throwError (LEEvalError EDInvalidArg pos)
primComparisonOp pos _ _ = throwError (LEEvalError EDWrongArgNumber pos)

primNot :: Position -> [Located EvalResult] -> Eval EvalResult
primNot _ [Located (RBool b) _] = return (RBool $ not b)
primNot _ [Located _ pos] = throwError (LEEvalError EDInvalidArg pos)
primNot pos _ = throwError (LEEvalError EDWrongArgNumber pos)

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
            when (length cArgs /= length argsExpr) $
                throwError (LEEvalError EDWrongArgNumber pos)

            args <- mapM eval argsExpr
            nestedEnv <- bindArgs cArgs args cEnv pos
            local (const nestedEnv) (eval cContent)
        _ -> throwError (LEEvalError EDInvalidFunction pos)
  where
    bindArgs :: [SymbolId] -> [EvalResult] -> Env -> Position -> Eval Env
    bindArgs ks vs env argPos
        | length ks /= length vs = throwError (LEEvalError EDWrongArgNumber argPos)
        | otherwise = return $ foldl' (\e (k, v) -> HM.insert k v e) env (zip ks vs)
