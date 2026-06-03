{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (

) where

import AST (Number (..), SExpr (..), SpecialSymbols (..), SymbolId (..))
import Control.Monad (when)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import LanguageError (EvalDetail (..), LangError (..))
import Location (Located (..), Position)

data Val
    = VNumber Number
    | VBool Bool
    | VStr T.Text
    | VList [Val]
    | VSExpr (Located SExpr)
    | VBinding SymbolId Val
    | VSpecialForm ([Located SExpr] -> Eval Val)
    | VPrimitive ([Val] -> Eval Val)
    | VClosure [SymbolId] (Located SExpr) Env

type Env = HM.HashMap SymbolId Val

newtype Eval a = Eval {runEval :: ReaderT Env (Except LangError) a}
    deriving (Monad, Applicative, Functor, MonadError LangError, MonadReader Env)

specialFormDefine :: Position -> [Located SExpr] -> Eval Val
specialFormDefine _ [Located (SSymbol sId) _, expr] = do
    val <- eval expr
    return (VBinding sId val)
specialFormDefine _ (Located _ symPos : _) = throwError (LEEvalError EDInvalidArg symPos)
specialFormDefine pos _ = throwError (LEEvalError EDWrongArgNumber pos)

specialFormIf :: Position -> [Located SExpr] -> Eval Val
specialFormIf _ [cond, conseq, alt] = do
    condVal <- eval cond
    case condVal of
        VBool False -> eval alt
        _ -> eval conseq
specialFormIf pos _ = throwError (LEEvalError EDWrongArgNumber pos)

specialFormLambda :: Position -> Env -> [Located SExpr] -> Eval Val
specialFormLambda _ env [Located (SList argList) _, body] = do
    idList <- getIds argList []
    return (VClosure idList body env)
  where
    getIds :: [Located SExpr] -> [SymbolId] -> Eval [SymbolId]
    getIds [] acc = return (reverse acc)
    getIds (Located (SSymbol sId) _ : xs) acc = getIds xs (sId : acc)
    getIds (Located _ pos : _) _ = throwError (LEEvalError EDInvalidArg pos)
specialFormLambda _ _ (Located _ argsPos : _) = throwError (LEEvalError EDInvalidArg argsPos)
specialFormLambda pos _ _ = throwError (LEEvalError EDWrongArgNumber pos)

specialFormLet :: Position -> [Located SExpr] -> Eval Val
specialFormLet _ [Located (SList bindings) _, body] = do
    bindVal <- parseBindings bindings []
    env <- ask
    let extendedEnv = foldl' (\e (k, v) -> HM.insert k v e) env bindVal
    local (const extendedEnv) (eval body)
  where
    parseBindings :: [Located SExpr] -> [(SymbolId, Val)] -> Eval [(SymbolId, Val)]
    parseBindings [] acc = return (reverse acc)
    parseBindings (Located (SList [Located (SSymbol sId) _, expr]) _ : xs) acc = do
        val <- eval expr
        parseBindings xs ((sId, val) : acc)
    parseBindings (Located (SList (Located _ sPos : _)) _ : _) _ = throwError (LEEvalError EDInvalidArg sPos)
    parseBindings (Located (SList _) listPos : _) _ = throwError (LEEvalError EDWrongArgNumber listPos)
    parseBindings (Located _ listPos : _) _ = throwError (LEEvalError EDInvalidArg listPos)
specialFormLet _ (Located _ argsPos : _) = throwError (LEEvalError EDInvalidArg argsPos)
specialFormLet pos _ = throwError (LEEvalError EDWrongArgNumber pos)

eval :: Located SExpr -> Eval Val
eval (Located (SNumber num) _) = return (VNumber num)
eval (Located (SBool boolean) _) = return (VBool boolean)
eval (Located (SStr str) _) = return (VStr str)
eval (Located (SSymbol sId) pos) = do
    env <- ask
    case HM.lookup sId env of
        Nothing -> throwError (LEEvalError EDUndefinedSymbol pos)
        Just val -> return val
eval (Located (SList []) _) = return (VList [])
eval (Located (SList (fnExpr : argsExpr)) pos) = do
    fnVal <- eval fnExpr
    case fnVal of
        VSpecialForm func -> func argsExpr
        VPrimitive primitive -> do
            argsVal <- mapM eval argsExpr
            primitive argsVal
        VClosure cArgs cContent cEnv -> do
            when (length cArgs /= length argsExpr) $
                throwError (LEEvalError EDWrongArgNumber pos)

            args <- mapM eval argsExpr
            nestedEnv <- bindArgs cArgs args cEnv pos
            local (const nestedEnv) (eval cContent)
        _ -> throwError (LEEvalError EDInvalidFunction pos)
eval (Located (SQuoted content) _) = return (VSExpr content)
eval (Located (SSpecialSymbol symbol) pos) = do
    env <- ask
    return $
        case symbol of
            PDefine -> VSpecialForm (specialFormDefine pos)
            PIf -> VSpecialForm (specialFormIf pos)
            PLambda -> VSpecialForm (specialFormLambda pos env)
            PLet -> VSpecialForm (specialFormLet pos)
            PAdd -> undefined
            PSub -> undefined
            PMul -> undefined
            PDiv -> undefined
            PEq -> undefined
            PGreaterThan -> undefined
            PLessThan -> undefined
            PNot -> undefined
            PCons -> undefined
            PCar -> undefined
            PCdr -> undefined
            PList -> undefined
            PNull -> undefined
            PDisplay -> undefined

bindArgs :: [SymbolId] -> [Val] -> Env -> Position -> Eval Env
bindArgs ks vs env pos
    | length ks /= length vs = throwError (LEEvalError EDWrongArgNumber pos)
    | otherwise = return $ foldl' (\e (k, v) -> HM.insert k v e) env (zip ks vs)
