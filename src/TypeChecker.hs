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
    variables :: [(String, TcType)]
}

type Check a = (StateT CheckState (Either String)) a

-- Monad boilerplate

checkTypes :: Program -> Either String Program
checkTypes prog = runChecker $ checkProgram prog

runChecker :: Check Program -> Either String Program
runChecker m = fmap (fst) $ runStateT m emptyCheckState

emptyCheckState :: CheckState
emptyCheckState = State {
    functions = [],
    currentFunction = "",
    variables = []
}


-- TcType checker implementation

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
    typedBlock <- checkBlock block
    return $ FnDef returnType (Ident name) args typedBlock

checkBlock :: Block -> Check Block
checkBlock (Blk stmts) = do
    vars <- gets variables
    typedStmts <- forM stmts checkStmt
    modify $ \s -> s { variables = vars }
    return $ Blk typedStmts

checkStmt :: Stmt -> Check Stmt
checkStmt Empty = return Empty
checkStmt (BStmt block) = do
    typedBlock <- checkBlock block
    return $ BStmt typedBlock
checkStmt (Decl varType items) = do
    typedItems <- forM items $ checkDecl (typeToTcType varType)
    return $ Decl varType typedItems
checkStmt (Ass (Ident name) expr) = do
    varType <- getVariableType name
    typedExpr <- expectType varType expr ("right hand side of assignment to " ++ name)
    return $ Ass (Ident name) typedExpr

checkStmt stmt @ (AbsLatte.Incr var @ (Ident name)) = do
    expectType TcInt (EVar var TcNone) ("variable " ++ name)
    return stmt
checkStmt stmt @ (AbsLatte.Decr var @ (Ident name)) = do
    expectType TcInt (EVar var TcNone) ("variable " ++ name)
    return stmt
checkStmt (Ret expr) = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (TcFun retType _) = currentFunction
    typedExpr <- expectType retType expr "return value"
    return $ Ret typedExpr
checkStmt VRet = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (TcFun retType _) = currentFunction
    when (retType /= TcVoid) $
        reportError ("function " ++ currentFunctionName ++ " has to return value")
    return VRet
checkStmt (Cond expr stmt) = do
    typedExpr <- expectType TcBool expr "if condition"
    typedStmt <- checkStmt stmt
    return $ Cond typedExpr typedStmt
checkStmt (CondElse expr stmtIf stmtElse) = do
    typedExpr     <- expectType TcBool expr "if condition"
    typedStmtIf   <- checkStmt stmtIf
    typedStmtElse <- checkStmt stmtElse
    return $ CondElse typedExpr typedStmtIf typedStmtElse
checkStmt (While expr stmt) = do
    typedExpr <- expectType TcBool expr "while condition"
    typedStmt <- checkStmt stmt
    return $ While typedExpr typedStmt
checkStmt (SExp expr) = do
    (typedExpr, _) <- checkExpr expr
    return $ SExp typedExpr

checkDecl :: TcType -> Item -> Check Item
checkDecl varType decl @ (NoInit (Ident name)) = do
    addVariable name varType
    return decl
checkDecl varType (Init (Ident name) expr) = do
    addVariable name varType
    typedExpr <- expectType varType expr ("initialization of variable " ++ name)
    return $ Init (Ident name) typedExpr

checkExpr :: Expr -> Check (Expr, TcType)
checkExpr (EVar (Ident name) _) = do
    varType <- getVariableType name
    return (EVar (Ident name) varType, varType)
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
beginFunction name = modify $ \s -> s { currentFunction = name }

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

getBuiltinFunctionType :: String -> Maybe TcType
getBuiltinFunctionType "printInt" = Just $ TcFun TcVoid [TcInt]
getBuiltinFunctionType "printString" = Just $ TcFun TcVoid [TcStr]
getBuiltinFunctionType "readInt" = Just $ TcFun TcInt []
getBuiltinFunctionType "error" = Just $ TcFun TcVoid []
getBuiltinFunctionType _ = Nothing

addArg :: Arg -> Check ()
addArg (Ar argType (Ident name)) = addVariable name (typeToTcType argType)

addVariable :: String -> TcType -> Check ()
addVariable name varType =  do
    variables <- gets variables
    modify $ \s -> s {
        variables = (name, varType):variables
    }

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

reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
