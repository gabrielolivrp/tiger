{-# LANGUAGE LambdaCase #-}

module Tiger.Semant where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Tiger.Semant.Env as E
import qualified Tiger.Semant.Symtab as S
import qualified Tiger.Semant.Translate as Trans
import qualified Tiger.Semant.Types as T
import Tiger.Semant.Unique
import qualified Tiger.Syntax.Parser.Ast as A
import Tiger.Syntax.Position

data SemantErrorKind
  = ExpectedIntType T.Ty -- Received
  | ExpectedUnitType T.Ty -- Received
  | UnexpectedVariable A.Symbol -- Var name
  | UnexpectedFunction A.Symbol -- Function name
  | UnboundVariable A.Symbol -- Var name
  | UnboundFunction A.Symbol -- Function name
  | UnboundType A.Symbol -- Type name
  | TypeMismatch T.Ty T.Ty -- Expected, Received
  | ArityMismatch Int Int -- Expected, Received
  | NamedTypeMismatch
  | ExpectedArrayTy T.Ty -- Received
  | ExpectedRecordTy T.Ty -- Received
  | UndefinedField
  | IllegalBreak
  deriving (Show)

data SemantError = SemantError
  { semErrKind :: !SemantErrorKind,
    semErrSpan :: !Span
  }
  deriving (Show)

data ExprTy = ExprTy
  { expr :: Trans.Expr,
    ty :: T.Ty
  }
  deriving (Show)

data SemantState = SemantState
  { tEnv :: E.TEnv,
    vEnv :: E.VEnv,
    unique :: Unique,
    hasLoop :: Bool
  }

type Semant a = StateT SemantState (Except SemantError) a

throwSemantError :: SemantErrorKind -> Span -> Semant a
throwSemantError errKind span = throwError (SemantError errKind span)

getVEnv :: Semant E.VEnv
getVEnv = gets vEnv

getTEnv :: Semant E.TEnv
getTEnv = gets tEnv

getNextUnique :: Semant Unique
getNextUnique = do
  u <- gets (nextUnique . unique)
  modify (\state -> state {unique = u})
  return u

transVar :: A.Var -> Semant ExprTy
transVar = \case
  A.SimpleVar span varName -> do
    venv <- getVEnv
    case S.look venv varName of
      Just var -> case var of
        E.VarEntry varTy -> return $ ExprTy () varTy
        _ -> throwSemantError (UnexpectedFunction varName) span
      Nothing -> throwSemantError (UnboundVariable varName) span
  A.FieldVar span var field -> do
    ExprTy {ty = varTy} <- transVar var
    case varTy of
      T.TyRecord fields _ ->
        case lookup field fields of
          Just fieldTy -> return $ ExprTy () fieldTy
          Nothing -> throwSemantError UndefinedField span
      ty -> throwSemantError NamedTypeMismatch span
  A.SubscriptVar span var indexExpr -> do
    ExprTy {ty = varTy} <- transVar var
    case varTy of
      T.TyArray arrayTy _ -> do
        ExprTy {ty = indexTy} <- transExpr indexExpr
        when (indexTy /= T.TyInt) $ throwSemantError (ExpectedIntType indexTy) span
        return $ ExprTy () arrayTy
      ty -> throwSemantError NamedTypeMismatch span

transExpr :: A.Expr -> Semant ExprTy
transExpr = \case
  A.EBreak span -> do
    loop <- gets hasLoop
    if loop
      then return $ ExprTy () T.TyUnit
      else throwSemantError IllegalBreak span
  A.ENil span -> return $ ExprTy () T.TyNil
  A.EInteger span n -> return $ ExprTy () T.TyInt
  A.EString span bs -> return $ ExprTy () T.TyString
  A.EFunctCall span name args -> do
    venv <- getVEnv
    case S.look venv name of
      Just (E.FunEntry formals resultTy) -> do
        args' <- mapM transExpr args

        let lenArgs = length args'
        let lenFormals = length formals
        when (lenArgs /= lenFormals) $ throwSemantError (ArityMismatch lenFormals lenArgs) span

        let params = zip args' formals
        forM_ params $ \(ExprTy {ty = argTy}, formTy) ->
          when (argTy /= formTy) $ throwSemantError (TypeMismatch formTy argTy) span

        return $ ExprTy () resultTy
      Just _ -> throwSemantError (UnexpectedVariable name) span
      Nothing -> throwSemantError (UnboundFunction name) span
  A.EOp span leftExpr op rightExpr -> do
    ExprTy {ty = leftTy} <- transExpr leftExpr
    ExprTy {ty = rightTy} <- transExpr rightExpr
    case (op, leftTy, rightTy) of
      (A.Plus, T.TyInt, T.TyInt) -> return ()
      (A.Minus, T.TyInt, T.TyInt) -> return ()
      (A.Times, T.TyInt, T.TyInt) -> return ()
      (A.Div, T.TyInt, T.TyInt) -> return ()
      (A.And, T.TyInt, T.TyInt) -> return ()
      (A.Or, T.TyInt, T.TyInt) -> return ()
      (A.Eq, T.TyInt, T.TyInt) -> return ()
      (A.Eq, T.TyString, T.TyString) -> return ()
      (A.Eq, T.TyRecord {}, T.TyRecord {}) -> return ()
      (A.Eq, T.TyArray {}, T.TyArray {}) -> return ()
      (A.NEq, T.TyInt, T.TyInt) -> return ()
      (A.NEq, T.TyString, T.TyString) -> return ()
      (A.NEq, T.TyRecord {}, T.TyRecord {}) -> return ()
      (A.NEq, T.TyArray {}, T.TyArray {}) -> return ()
      (A.Lt, T.TyInt, T.TyInt) -> return ()
      (A.Le, T.TyInt, T.TyInt) -> return ()
      (A.Gt, T.TyInt, T.TyInt) -> return ()
      (A.Ge, T.TyInt, T.TyInt) -> return ()
      (A.Lt, T.TyString, T.TyString) -> return ()
      (A.Le, T.TyString, T.TyString) -> return ()
      (A.Gt, T.TyString, T.TyString) -> return ()
      (A.Ge, T.TyString, T.TyString) -> return ()
      _ -> throwSemantError (TypeMismatch leftTy rightTy) span
    return $ ExprTy () T.TyInt
  A.EIf span pred conseq alt -> do
    ExprTy {ty = predTy} <- transExpr pred
    ExprTy {ty = conseqTy} <- transExpr conseq
    when (predTy /= T.TyInt) $ throwSemantError (ExpectedIntType predTy) span
    case alt of
      Just alt' -> do
        ExprTy {ty = altTy} <- transExpr alt'
        when (conseqTy /= altTy) $ throwSemantError (TypeMismatch conseqTy altTy) span
        return $ ExprTy () conseqTy
      Nothing -> return $ ExprTy () conseqTy
  A.EWhile span testExpr body -> do
    ExprTy {ty = testTy} <- transExpr testExpr
    when (testTy /= T.TyInt) $ throwSemantError (ExpectedIntType testTy) span
    withStateT (\state -> state {hasLoop = True}) $ do
      ExprTy {ty = bodyTy} <- transExpr body
      when (bodyTy /= T.TyUnit) $ throwSemantError (ExpectedUnitType bodyTy) span
      return $ ExprTy () T.TyUnit
  A.EFor span var loExpr hiExpr body -> do
    venv <- getVEnv
    ExprTy {ty = loTy} <- transExpr loExpr
    ExprTy {ty = hiTy} <- transExpr hiExpr
    when (loTy /= T.TyInt) $ throwSemantError (ExpectedIntType loTy) span
    when (hiTy /= T.TyInt) $ throwSemantError (ExpectedIntType hiTy) span
    let venv' = S.enter venv var (E.VarEntry T.TyInt)
    withStateT (\state -> state {hasLoop = True, vEnv = venv}) $ do
      ExprTy {ty = bodyTy} <- transExpr body
      when (bodyTy /= T.TyUnit) $ throwSemantError (ExpectedUnitType bodyTy) span
      return $ ExprTy () T.TyUnit
  A.EAssign span var body -> do
    ExprTy {ty = varTy} <- transVar var
    ExprTy {ty = bodyTy} <- transExpr body
    when (varTy /= bodyTy) $ throwSemantError (TypeMismatch varTy bodyTy) span
    return $ ExprTy () varTy
  A.ELet span decs body -> do
    (venv, tenv) <- transDecs decs
    withStateT (\state -> state {vEnv = venv, tEnv = tenv}) (transExpr body)
  A.ESeq _ exprs -> do
    exprsTy <- forM exprs transExpr
    return $
      if null exprsTy
        then ExprTy () T.TyUnit
        else ExprTy () $ (ty . last) exprsTy
  A.EArray span name size init -> do
    tenv <- getTEnv
    case S.look tenv name of
      Just arrayTy@(T.TyArray elemTy _) -> do
        ExprTy {ty = sizeTy} <- transExpr size
        ExprTy {ty = initTy} <- transExpr init
        when (sizeTy /= T.TyInt) (throwSemantError (TypeMismatch T.TyInt sizeTy) span)
        when (elemTy /= initTy) (throwSemantError (TypeMismatch elemTy initTy) span)
        return $ ExprTy () arrayTy
      Just ty -> throwSemantError (ExpectedArrayTy ty) span
      Nothing -> throwSemantError (UnboundType name) span
  A.ERecord span name fields -> do
    tenv <- getTEnv
    case S.look tenv name of
      Just recordTy@(T.TyRecord fieldsTy _) -> do
        let fieldsLen = length fields
        let fieldsTyLen = length fieldsTy
        when (fieldsLen /= fieldsTyLen) $ throwSemantError (ArityMismatch fieldsLen fieldsTyLen) span
        forM_ fieldsTy $ \(field, fieldty) -> do
          case lookup field fields of
            Nothing -> throwSemantError (UnboundVariable field) span
            Just expr -> do
              ExprTy {ty = exprTy} <- transExpr expr
              when (fieldty /= exprTy) $ throwSemantError (TypeMismatch fieldty exprTy) span
        return $ ExprTy () recordTy
      Just ty -> throwSemantError (ExpectedRecordTy ty) span
      Nothing -> throwSemantError (UnboundType name) span
  A.EVar span var -> transVar var

transDecs :: [A.Dec] -> Semant (E.VEnv, E.TEnv)
transDecs decs = do
  venv <- gets vEnv
  tenv <- gets tEnv
  foldM go (venv, tenv) decs
  where
    go (venv, tenv) dec =
      withStateT (\state -> state {tEnv = tenv, vEnv = venv}) (transDec dec)

transDec :: A.Dec -> Semant (E.VEnv, E.TEnv)
transDec = \case
  A.TypeDec span name ty -> do
    venv <- gets vEnv
    tenv <- gets tEnv
    ty' <- transTy ty
    let tenv' = S.enter tenv name ty'
    return (venv, tenv')
  A.VarDec span name decTy body -> do
    ExprTy {ty = bodyTy} <- transExpr body
    venv <- gets vEnv
    tenv <- gets tEnv
    when (isJust decTy) $ do
      let decTy' = fromJust decTy
      case S.look tenv decTy' of
        Nothing -> throwSemantError (UnboundType name) span
        Just decTy' ->
          when (bodyTy /= decTy') $ throwSemantError (TypeMismatch decTy' bodyTy) span

    let venv' = S.enter venv name (E.VarEntry bodyTy)
    let tenv' = S.enter tenv name bodyTy
    return (venv', tenv')
  A.FunctDec span name params resultTy body -> do
    venv <- getVEnv
    tenv <- getTEnv
    params' <- transFields params
    resultTy' <- case resultTy of
      Just ty -> case S.look tenv ty of
        Nothing -> throwSemantError (UnboundType ty) span
        Just resultTy' -> return resultTy'
      Nothing -> return T.TyUnit
    let formals = map snd params'
    let venv' = S.enter venv name (E.FunEntry formals resultTy')
    let venv'' = foldr enterParam venv' params'
    withStateT (\state -> state {vEnv = venv''}) $ do
      ExprTy {ty = bodyTy} <- transExpr body
      when (bodyTy /= resultTy') (throwSemantError (TypeMismatch resultTy' bodyTy) span)
    return (venv', tenv)
  where
    enterParam (name, ty) venv = S.enter venv name (E.VarEntry ty)

transTy :: A.Ty -> Semant T.Ty
transTy = \case
  A.TyName span ty -> do
    tenv <- gets tEnv
    case S.look tenv ty of
      Just ty' -> return ty'
      Nothing -> throwSemantError (UnboundType ty) span
  A.TyRecord _ fields -> do
    params <- transFields fields
    T.TyRecord params <$> getNextUnique
  A.TyArray span ty -> do
    tenv <- gets tEnv
    case S.look tenv ty of
      Just ty' -> T.TyArray ty' <$> getNextUnique
      Nothing -> throwSemantError (UnboundType ty) span

transFields :: [A.TyField] -> Semant [(A.Symbol, T.Ty)]
transFields fields = do
  tenv <- getTEnv
  forM fields $ \(A.TyField span name ty) -> do
    case S.look tenv ty of
      Just ty' -> return (name, ty')
      Nothing -> throwSemantError (UnboundType ty) span

transProg :: A.Expr -> Either SemantError ()
transProg expr =
  let state = SemantState E.baseTEnv E.baseVEnv (Unique 0) False
      semant = transExpr expr
   in case runExcept $ runStateT semant state of
        Left err -> Left err
        Right _ -> Right ()
