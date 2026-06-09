{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval (
    EvalResult (..),
    Env,
    EvalCtx (..),
    Eval (..),
    initialEnv,
    eval,
) where

import AST (SExpr (..))
import Control.Applicative ((<|>))
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import LanguageError (ArgNumMismatch (..), EvalDetail (..), LangError (..), TypeMismatch (..))
import Location (Located (..), Position)
import Numbers (Number (..), compareNums)
import Symbols

data EvalResult
    = RNumber Number
    | RBool Bool
    | RStr T.Text
    | RList [EvalResult]
    | RSymbol SymbolId
    | RBinding SymbolId EvalResult
    | RPrint EvalResult
    | RSpecialForm SpecialForm
    | RPrimitive Primitive
    | RClosure [SymbolId] (Located SExpr) Env

type Env = HM.HashMap SymbolId EvalResult

type SpecialForm = Position -> Env -> [Located SExpr] -> Eval EvalResult
type Primitive = Position -> [Located EvalResult] -> Eval EvalResult

data EvalCtx = EvalCtx
    { eLocal :: Env
    , eGlobal :: Env
    }

newtype Eval a = Eval {runEval :: ReaderT EvalCtx (Except LangError) a}
    deriving (Monad, Applicative, Functor, MonadError LangError, MonadReader EvalCtx)

getSExprTypeName :: SExpr -> T.Text
getSExprTypeName (SNumber _) = "number"
getSExprTypeName (SBool _) = "boolean"
getSExprTypeName (SStr _) = "string"
getSExprTypeName (SList _) = "list"
getSExprTypeName (SSymbol _) = "symbol"

getEvalResultTypeName :: EvalResult -> T.Text
getEvalResultTypeName (RNumber _) = "number"
getEvalResultTypeName (RBool _) = "boolean"
getEvalResultTypeName (RStr _) = "string"
getEvalResultTypeName (RList _) = "list"
getEvalResultTypeName (RSymbol _) = "symbol"
getEvalResultTypeName (RBinding _ _) = "<binding>"
getEvalResultTypeName (RPrint _) = "<display>"
getEvalResultTypeName (RSpecialForm _) = "<special form>"
getEvalResultTypeName (RPrimitive _) = "<primitive>"
getEvalResultTypeName (RClosure _ _ _) = "<closure>"

initialEnv :: Env
initialEnv =
    HM.fromList
        [ (symQuote, RSpecialForm specialFormQuote)
        , (symDefine, RSpecialForm specialFormDefine)
        , (symIf, RSpecialForm specialFormIf)
        , (symLambda, RSpecialForm specialFormLambda)
        , (symLet, RSpecialForm specialFormLet)
        , (symPlus, RPrimitive (primCommutativeOp (+) 0))
        , (symMinus, RPrimitive (primArithmeticOp "-" 0 (-) True))
        , (symMult, RPrimitive (primCommutativeOp (*) 1))
        , (symDiv, RPrimitive (primArithmeticOp "/" 1 (/) False))
        , (symEq, RPrimitive (primComparisonOp "=" EQ))
        , (symGt, RPrimitive (primComparisonOp ">" GT))
        , (symLt, RPrimitive (primComparisonOp "<" LT))
        , (symNot, RPrimitive primNot)
        , (symCons, RPrimitive primCons)
        , (symCar, RPrimitive primCar)
        , (symCdr, RPrimitive primCdr)
        , (symList, RPrimitive primList)
        , (symIsNull, RPrimitive primNull)
        , (symDisplay, RPrimitive primDisplay)
        ]

specialFormQuote :: SpecialForm
specialFormQuote pos env args = case args of
    [Located (SNumber num) _] -> return (RNumber num)
    [Located (SBool boolean) _] -> return (RBool boolean)
    [Located (SSymbol sId) _] -> return (RSymbol sId)
    [Located (SStr content) _] -> return (RStr content)
    [Located (SList elements) _] -> do
        content <- mapM (\e -> specialFormQuote pos env [e]) elements
        return (RList content)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "quote") 1 (length args))) pos)

specialFormDefine :: SpecialForm
specialFormDefine pos _ args = case args of
    [Located (SSymbol sId) _, expr] -> do
        val <- eval expr
        return (RBinding sId val)
    [Located nonSym symPos, _] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "symbol" (getSExprTypeName nonSym))) symPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "define") 2 (length args))) pos)

specialFormIf :: SpecialForm
specialFormIf pos _ args = case args of
    [cond, conseq, alt] -> do
        condVal <- eval cond
        case condVal of
            RBool False -> eval alt
            _ -> eval conseq
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "if") 3 (length args))) pos)

specialFormLambda :: SpecialForm
specialFormLambda pos env args = case args of
    [Located (SList argList) _, body] -> do
        idList <- getIds argList []
        return (RClosure idList body env)
    [Located nonList argsPos, _] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getSExprTypeName nonList))) argsPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "lambda") 2 (length args))) pos)
  where
    getIds :: [Located SExpr] -> [SymbolId] -> Eval [SymbolId]
    getIds exprs acc = case exprs of
        [] -> return (reverse acc)
        (Located (SSymbol sId) _ : xs) -> getIds xs (sId : acc)
        (Located nonSym idPos : _) -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "symbol" (getSExprTypeName nonSym))) idPos)

-- Behaves more like `let*` rather than `let` from Racket
specialFormLet :: SpecialForm
specialFormLet pos _ args = case args of
    [Located (SList bindings) _, body] -> do
        EvalCtx localEnv globalEnv <- ask
        extendedEnv <- parseBindings bindings localEnv
        local (const $ EvalCtx extendedEnv globalEnv) (eval body)
    [Located nonList argsPos, _] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getSExprTypeName nonList))) argsPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "let") 2 (length args))) pos)
  where
    parseBindings :: [Located SExpr] -> Env -> Eval Env
    parseBindings b acc = case b of
        [] -> return acc
        (Located (SList [Located (SSymbol sId) _, expr]) _ : xs) -> do
            ctx <- ask
            val <- local (const ctx{eLocal = acc}) (eval expr)
            parseBindings xs (HM.insert sId val acc)
        (Located (SList (Located nonSym sPos : _)) _ : _) -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "symbol" (getSExprTypeName nonSym))) sPos)
        (Located (SList argsList) listPos : _) -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "let-binding") 2 (length argsList))) listPos)
        (Located nonList listPos : _) -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getSExprTypeName nonList))) listPos)

primCommutativeOp :: (Number -> Number -> Number) -> Number -> Primitive
primCommutativeOp op acc pos args = case args of
    [] -> return (RNumber acc)
    (Located (RNumber num) _ : cs) -> primCommutativeOp op (num `op` acc) pos cs
    (Located nonNum argPos : _) ->
        throwError (LEEvalError (EDTypeMismatch (TypeMismatch "number" (getEvalResultTypeName nonNum))) argPos)

primArithmeticOp :: T.Text -> Number -> (Number -> Number -> Number) -> Bool -> Primitive
primArithmeticOp opName base operation allowZero pos args = case args of
    [] -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just opName) 1 0)) pos)
    [Located val argPos] -> case val of
        RNumber num ->
            if not allowZero && isZero num
                then throwError (LEEvalError EDDivisionByZero argPos)
                else return (RNumber $ base `operation` num)
        _ -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "number" (getEvalResultTypeName val))) argPos)
    (Located firstVal firstPos : cs) -> case firstVal of
        RNumber firstNum -> do
            res <- calculate cs firstNum
            return (RNumber res)
        _ -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "number" (getEvalResultTypeName firstVal))) firstPos)
  where
    isZero (NInt 0) = True
    isZero (NFloat 0.0) = True
    isZero _ = False

    calculate :: [Located EvalResult] -> Number -> Eval Number
    calculate [] acc = return acc
    calculate (Located (RNumber x) numPos : xs) acc
        | not allowZero && isZero x = throwError (LEEvalError EDDivisionByZero numPos)
        | otherwise = calculate xs (acc `operation` x)
    calculate (Located nonNum argPos : _) _ = throwError (LEEvalError (EDTypeMismatch (TypeMismatch "number" (getEvalResultTypeName nonNum))) argPos)

primComparisonOp :: T.Text -> Ordering -> Primitive
primComparisonOp opName ordering pos args = case args of
    [Located val1 pos1, Located val2 pos2] -> case (val1, val2) of
        (RNumber a, RNumber b) -> return (RBool $ compareNums a b == ordering)
        (RNumber _, nonNum) -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "number" (getEvalResultTypeName nonNum))) pos2)
        (nonNum, _) -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "number" (getEvalResultTypeName nonNum))) pos1)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just opName) 2 (length args))) pos)

primNot :: Primitive
primNot pos args = case args of
    [Located (RBool b) _] -> return (RBool $ not b)
    [Located val argPos] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "boolean" (getEvalResultTypeName val))) argPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "not") 1 (length args))) pos)

primCons :: Primitive
primCons pos args = case args of
    [Located x _, Located (RList xs) _] -> return (RList (x : xs))
    [Located _ _, Located val argPos] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getEvalResultTypeName val))) argPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "cons") 2 (length args))) pos)

primCar :: Primitive
primCar pos args = case args of
    [Located (RList (c : _)) _] -> return c
    [Located (RList []) argPos] -> throwError (LEEvalError (EDEmptyListOperation "car") argPos)
    [Located val argPos] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getEvalResultTypeName val))) argPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "car") 1 (length args))) pos)

primCdr :: Primitive
primCdr pos args = case args of
    [Located (RList (_ : cs)) _] -> return (RList cs)
    [Located (RList []) argPos] -> throwError (LEEvalError (EDEmptyListOperation "cdr") argPos)
    [Located val argPos] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getEvalResultTypeName val))) argPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "cdr") 1 (length args))) pos)

primList :: Primitive
primList _ args = return (RList [x | Located x _ <- args])

primNull :: Primitive
primNull pos args = case args of
    [Located (RList content) _] -> return (RBool $ null content)
    [Located val argPos] -> throwError (LEEvalError (EDTypeMismatch (TypeMismatch "list" (getEvalResultTypeName val))) argPos)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "null?") 1 (length args))) pos)

primDisplay :: Primitive
primDisplay pos args = case args of
    [Located val _] -> return (RPrint val)
    _ -> throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch (Just "display") 1 (length args))) pos)

eval :: Located SExpr -> Eval EvalResult
eval (Located expr pos) = case expr of
    SNumber num -> return (RNumber num)
    SBool boolean -> return (RBool boolean)
    SStr str -> return (RStr str)
    SSymbol sId -> do
        EvalCtx localEnv globalEnv <- ask
        case HM.lookup sId localEnv <|> HM.lookup sId globalEnv of
            Nothing -> throwError (LEEvalError (EDUndefinedSymbol sId) pos)
            Just val -> return val
    SList [] -> return (RList [])
    SList (fnExpr@(Located _ fnPos) : argsExpr) -> do
        env@(EvalCtx localEnv _) <- ask
        fnVal <- eval fnExpr
        case fnVal of
            RSpecialForm func -> func fnPos localEnv argsExpr
            RPrimitive primitive -> do
                vals <- mapM eval argsExpr
                let argsVal = zipWith (\v (Located _ exprPos) -> Located v exprPos) vals argsExpr
                primitive fnPos argsVal
            RClosure cArgs cContent cEnv -> do
                args <- mapM eval argsExpr
                nestedEnv <- bindArgs cArgs args cEnv pos
                local (const $ env{eLocal = nestedEnv}) (eval cContent)
            _ -> throwError (LEEvalError (EDNotAFunction (getEvalResultTypeName fnVal)) pos)
  where
    bindArgs :: [SymbolId] -> [EvalResult] -> Env -> Position -> Eval Env
    bindArgs ks vs env argPos
        | length ks /= length vs =
            throwError (LEEvalError (EDWrongArgNumber (ArgNumMismatch Nothing (length ks) (length vs))) argPos)
        | otherwise = return $ foldl' (\e (k, v) -> HM.insert k v e) env (zip ks vs)
