{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (

) where

import AST (Number (..), SExpr (..), SymbolId (..))
import Control.Monad.Except (Except, MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import LanguageError (EvalDetail (..), LangError (..))
import Location (Located (..))
import Control.Monad (when)

data Val
    = VNumber Number
    | VBool Bool
    | VStr T.Text
    | VList [Val]
    | VSExpr (Located SExpr)
    | VPrimitive ([Val] -> Eval Val)
    | VClosure [SymbolId] (Located SExpr) Env

type Env = HM.HashMap SymbolId Val

newtype Eval a = Eval {runEval :: Except LangError a}
    deriving (Monad, Applicative, Functor, MonadError LangError)

eval :: Env -> Located SExpr -> Eval Val
eval _ (Located (SNumber num) _) = return (VNumber num)
eval _ (Located (SBool boolean) _) = return (VBool boolean)
eval _ (Located (SStr str) _) = return (VStr str)
eval env (Located (SSymbol sId) pos) = do
    case HM.lookup sId env of
        Nothing -> throwError (LEEvalError EDUndefinedSymbol pos)
        Just val -> return val
eval _ (Located (SList []) _) = return (VList [])
eval env (Located (SList (fnExpr : argsExpr)) pos) = do
    fnVal <- eval env fnExpr
    case fnVal of
        VPrimitive primitive -> do
            args <- mapM (eval env) argsExpr
            primitive args
        VClosure cArgs cContent cEnv -> do
            when (length cArgs /= length argsExpr) $
                throwError (LEEvalError EDWrongArgNumber pos)

            args <- mapM (eval env) argsExpr
            let nestedEnv = HM.union (HM.fromList (zip cArgs args)) cEnv
            eval nestedEnv cContent
        _ -> throwError (LEEvalError EDInvalidFunction pos)
eval _ (Located (SQuoted content) _) = return (VSExpr content)
