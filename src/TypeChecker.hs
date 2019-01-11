module TypeChecker (checkTypes) where

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List

data CheckState = State {
    functions :: [(String, TcType)],
    currentFunction :: String,
    variables :: [(String, TcType)],
    innerVariables :: [(String)],
    returned :: Bool
}

type Check a = (StateT CheckState (Either String)) a

-- Monad boilerplate

checkTypes :: Program -> Either String Program
checkTypes prog = runChecker $ checkProgram prog

runChecker :: Check Program -> Either String Program
runChecker m = fmap fst $ runStateT m emptyCheckState

emptyCheckState :: CheckState
emptyCheckState = State {
    functions = [],
    currentFunction = "",
    variables = [],
    innerVariables = [],
    returned = False
}


-- Type checker implementation

checkProgram :: Program -> Check Program
checkProgram (Prog topdefs) = do
    forM_ topdefs addTopDef
    typedTopDefs <- forM topdefs checkTopDef
    return $ Prog typedTopDefs

addTopDef :: TopDef -> Check ()
addTopDef (FnDef returnType (Ident name) args _) =
    let argsTypes = map (\(Ar argType _) -> (typeToTcType argType)) args in
    addFunction name (TcFun (typeToTcType returnType) argsTypes)

checkTopDef :: TopDef -> Check TopDef
checkTopDef (FnDef returnType (Ident name) args block) = do
    beginFunction name
    forM_ args addArg
    typedBlock  <- checkBlock block
    funReturned <- gets returned
    when (not funReturned && returnType /= Void) $
        reportError ("Function " ++ name ++ " may not return value")
    return $ FnDef returnType (Ident name) args typedBlock

checkBlock :: Block -> Check Block
checkBlock (Blk stmts) = do
    vars <- gets variables
    innerVars <- gets innerVariables
    modify $ \s -> s { innerVariables = [] }
    typedStmts <- forM stmts checkStmt
    modify $ \s -> s { variables = vars, innerVariables = innerVars }
    return $ Blk typedStmts

checkStmt :: Stmt -> Check Stmt
checkStmt Empty = return Empty
checkStmt (BStmt block) = do
    typedBlock <- checkBlock block
    return $ BStmt typedBlock
checkStmt (Decl varType items) = do
    typedItems <- forM items $ checkDecl (typeToTcType varType)
    setReturned False
    return $ Decl varType typedItems
checkStmt (Ass lval expr) = do
    (typedLVal, lvalType) <- checkLVal lval
    typedExpr <- expectType lvalType expr ("right hand side of assignment")
    setReturned False
    return $ Ass typedLVal typedExpr
checkStmt (AbsLatte.Incr lval) = do
    typedExpr <- expectType TcInt (ELVal lval TcNone) "lvalue"
    let (ELVal typedLval _) = typedExpr
    setReturned False
    return $ AbsLatte.Incr typedLval
checkStmt (AbsLatte.Decr lval) = do
    typedExpr <- expectType TcInt (ELVal lval TcNone) "lvalue"
    let (ELVal typedLval _) = typedExpr
    setReturned False
    return $ AbsLatte.Decr typedLval
checkStmt (Ret expr) = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (TcFun retType _) = currentFunction
    typedExpr <- expectType retType expr "return value"
    setReturned True
    return $ Ret typedExpr
checkStmt VRet = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (TcFun retType _) = currentFunction
    when (retType /= TcVoid) $
        reportError ("function " ++ currentFunctionName ++ " has to return value")
    setReturned False
    return VRet
checkStmt (Cond expr stmt) = do
    typedExpr <- expectType TcBool expr "if condition"
    setReturned False
    typedStmt <- checkStmt stmt
    returned  <- gets returned
    case exprEval expr of
        Just True -> setReturned returned
        _         -> setReturned False
    return $ Cond typedExpr typedStmt
checkStmt (CondElse expr stmtIf stmtElse) = do
    typedExpr     <- expectType TcBool expr "if condition"
    setReturned False
    typedStmtIf   <- checkStmt stmtIf
    ifReturned    <- gets returned
    setReturned False
    typedStmtElse <- checkStmt stmtElse
    elseReturned  <- gets returned
    case exprEval expr of
        Just True  -> setReturned ifReturned
        Just False -> setReturned elseReturned
        Nothing    -> setReturned (ifReturned && elseReturned)
    return $ CondElse typedExpr typedStmtIf typedStmtElse
checkStmt (While expr stmt) = do
    typedExpr <- expectType TcBool expr "while condition"
    typedStmt <- checkStmt stmt
    return $ While typedExpr typedStmt
checkStmt (SExp expr) = do
    (typedExpr, _) <- checkExpr expr
    setReturned False
    return $ SExp typedExpr

checkDecl :: TcType -> Item -> Check Item
checkDecl varType decl @ (NoInit (Ident name)) = do
    addVariable name varType
    return decl
checkDecl varType (Init (Ident name) expr) = do
    addVariable name varType
    typedExpr <- expectType varType expr ("initialization of variable " ++ name)
    return $ Init (Ident name) typedExpr

checkLVal :: LVal -> Check (LVal, TcType)
checkLVal (ObjField lval (Ident fieldName) _) = do
    (typedLval, lvalType) <- checkLVal lval
    innerType <- getFieldType lvalType fieldName
    return (ObjField typedLval (Ident fieldName) innerType, innerType)
checkLVal (Var (Ident name) _) = do
    varType <- getVariableType name
    return (Var (Ident name) varType, varType)
checkLVal (ArrElem (Ident name) indexExpr _) = do
    typedIndexExpr <- expectType TcInt indexExpr "array index"
    varType <- getVariableType name
    let (TcArr innerType) = varType
    return (ArrElem (Ident name) typedIndexExpr innerType, innerType)

checkExpr :: Expr -> Check (Expr, TcType)
checkExpr (EArrNew arrType size _) = do
    typedSize <- expectType TcInt size "new array length"
    return (EArrNew arrType typedSize exprType, exprType) where
    exprType = TcArr (typeToTcType arrType)
checkExpr (ELVal lval _) = do
    (typedLval, lvalType) <- checkLVal lval
    return (ELVal typedLval lvalType, lvalType)
checkExpr (ELitInt n _) = return (ELitInt n TcInt, TcInt)
checkExpr (ELitTrue _) = return (ELitTrue TcBool, TcBool)
checkExpr (ELitFalse _) = return (ELitFalse TcBool, TcBool)
checkExpr (EApp (Ident funName) args _) = do
    funType <- getFunctionType funName
    let (TcFun retType funArgTypes) = funType
    when ((length args) /= (length funArgTypes)) $
        reportError ("Incorrect number of arguments for function " ++ funName)
    checkedArgs <- forM args checkExpr
    let typedArgs = map fst checkedArgs
    let argTypes  = map snd checkedArgs
    when (argTypes /= funArgTypes) $ reportError
        ("Argument types for function " ++ funName ++ " does not match")
    return (EApp (Ident funName) typedArgs retType, retType)
checkExpr (EString string _) = return (EString string TcStr, TcStr)
checkExpr (Neg expr _) = do
    typedExpr <- expectType TcInt expr "negation argument"
    return (Neg typedExpr TcInt, TcInt)
checkExpr (Not expr _) = do
    typedExpr <- expectType TcBool expr "negation argument"
    return (Not typedExpr TcBool, TcBool)
checkExpr (EMul lhs mulop rhs _) = do
    typedLhs <- expectType TcInt lhs "left multiplication operand"
    typedRhs <- expectType TcInt rhs "right multiplication operand"
    return (EMul typedLhs mulop typedRhs TcInt, TcInt)
checkExpr (EAdd lhs addop rhs _) = do
    checkedLhs <- checkExpr lhs
    let lhsType  = snd checkedLhs
    if lhsType == TcInt then do
        typedLhs <- expectType TcInt lhs "left addition operand"
        typedRhs <- expectType TcInt rhs "right addition operand"
        return (EAdd typedLhs addop typedRhs TcInt, TcInt)
    else if lhsType == TcStr then do
        typedLhs <- expectType TcStr lhs "left addition operand"
        typedRhs <- expectType TcStr rhs "right addition operand"
        return (EAdd typedLhs addop typedRhs TcStr, TcStr)
    else reportError
        ("Incorrect type of left addidtion operand: expected " ++ show TcInt ++ " or " ++
            show TcStr ++ ", got " ++ show lhsType)

checkExpr (ERel lhs relop rhs _) = do
    checkedLhs <- checkExpr lhs
    let lhsType  = snd checkedLhs
    if (lhsType == TcBool && (relop == EQU || relop == NE)) then do
        typedLhs <- expectType TcBool lhs "left comparison operand"
        typedRhs <- expectType TcBool rhs "right comparison operand"
        return (ERel typedLhs relop typedRhs TcBool, TcBool)
    else do
        typedLhs <- expectType TcInt lhs "left comparison operand"
        typedRhs <- expectType TcInt rhs "right comparison operand"
        return (ERel typedLhs relop typedRhs TcBool, TcBool)
checkExpr (EAnd lhs rhs _) = do
    typedLhs <- expectType TcBool lhs "left and operand"
    typedRhs <- expectType TcBool rhs "right and operand"
    return (EAnd typedLhs typedRhs TcBool, TcBool)
checkExpr (EOr lhs rhs _) = do
    typedLhs <- expectType TcBool lhs "left and operand"
    typedRhs <- expectType TcBool rhs "right and operand"
    return (EOr typedLhs typedRhs TcBool, TcBool)

beginFunction :: String -> Check ()
beginFunction name = modify $ \s -> s { currentFunction = name, variables = [], innerVariables = [] }

-- Constant evaluation (only boolean for now)

exprEval :: Expr -> Maybe Bool
exprEval (ELitTrue _) = Just True
exprEval (ELitFalse _) = Just False
exprEval (EAnd lhs rhs _) = case (exprEval lhs, exprEval rhs) of
    (Just True, Just True) -> Just True
    (Just False, _)        -> Just False
    (_, Just False)        -> Just False
    (_, _)                 -> Nothing
exprEval (EOr lhs rhs _) = case (exprEval lhs, exprEval rhs) of
    (Just False, Just False) -> Just False
    (Just True, _)           -> Just True
    (_, Just True)           -> Just True
    (_, _)                   -> Nothing
exprEval _ = Nothing

-- Auxiliary functions

addFunction :: String -> TcType -> Check ()
addFunction name funType =  do
    functions <- gets functions
    modify $ \s -> s {
        functions = (name, funType):functions
    }

getFunctionType :: String -> Check TcType
getFunctionType name = do
    case getBuiltinFunctionType name of
        Just funType -> return funType
        Nothing -> do
            function <- gets functions
            case lookup name function of
                Just funType -> return funType
                Nothing      -> reportError $ "Unkown function " ++ name

getFieldType :: TcType -> String -> Check TcType
getFieldType (TcArr _) "length" = return TcInt
getFieldType _ fieldName = reportError ("unknown field "  ++ fieldName)

getBuiltinFunctionType :: String -> Maybe TcType
getBuiltinFunctionType "printInt" = Just $ TcFun TcVoid [TcInt]
getBuiltinFunctionType "printString" = Just $ TcFun TcVoid [TcStr]
getBuiltinFunctionType "readInt" = Just $ TcFun TcInt []
getBuiltinFunctionType "readString" = Just $ TcFun TcStr []
getBuiltinFunctionType "error" = Just $ TcFun TcVoid []
getBuiltinFunctionType _ = Nothing

addArg :: Arg -> Check ()
addArg (Ar argType (Ident name)) = do
    variables <- gets variables
    case lookup name variables of
        Just _  -> reportError $ "Repeated argument name " ++ name
        Nothing -> addVariable name (typeToTcType argType)

addVariable :: String -> TcType -> Check ()
addVariable name varType =  do
    variables <- gets variables
    innerVariables <- gets innerVariables
    when (elem name innerVariables) (reportError $ "Multiple declarations of variable " ++ name)
    modify $ \s -> s {
            variables = (name, varType):variables,
            innerVariables = name:innerVariables
        }

setReturned :: Bool -> Check ()
setReturned ret = modify $ \s -> s { returned = ret}

getVariableType :: String -> Check TcType
getVariableType name = do
    variables <- gets variables
    case lookup name variables of
        Just varType -> return varType
        Nothing      -> reportError $ "Unkown variable " ++ name

expectType :: TcType -> Expr -> String -> Check Expr
expectType expectedType expr what = do
    checkedExpr <- checkExpr expr
    let typedExpr  = fst checkedExpr
    let actualType = snd checkedExpr
    when (expectedType /= actualType) $ reportError ("Incorrect type of " ++ what ++
        ": expected " ++ (show expectedType) ++ ", got " ++ (show actualType))
    return typedExpr

typeToTcType :: Type -> TcType
typeToTcType Int  = TcInt
typeToTcType Str  = TcStr
typeToTcType Bool = TcBool
typeToTcType Void = TcVoid
typeToTcType (Arr arrType)  = TcArr (typeToTcType arrType)

reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
