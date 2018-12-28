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

checkTypes :: Program -> Either String ()
checkTypes prog = runChecker $ checkProgram prog

runChecker :: Check () -> Either String ()
runChecker m = fmap (const ()) $ runStateT m emptyCheckState

emptyCheckState :: CheckState
emptyCheckState = State {
    functions = [],
    currentFunction = "",
    variables = []
}


-- TcType checker implementation

checkProgram :: Program -> Check ()
checkProgram (Prog topdefs) = do
    forM_ topdefs addTopDef
    forM_ topdefs checkTopDef

addTopDef :: TopDef -> Check ()
addTopDef (FnDef returnType (Ident name) args _) =
    let argsTypes = map (\(Ar argType _) -> (typeToTcType argType)) args in
    addFunction name (TcFun (typeToTcType returnType) argsTypes)

checkTopDef :: TopDef -> Check()
checkTopDef (FnDef returnType (Ident name) args block) = do
    beginFunction name
    forM_ args addArg
    checkBlock block

checkBlock :: Block -> Check ()
checkBlock (Blk stmts) = do
    vars <- gets variables
    forM_ stmts checkStmt
    modify $ \s -> s { variables = vars }

checkStmt :: Stmt -> Check ()
checkStmt Empty = return ()
checkStmt (BStmt block) = checkBlock block
checkStmt (Decl varType items) = forM_ items $ checkDecl (typeToTcType varType)
checkStmt (Ass (Ident name) expr) = do
    varType <- getVariableType name
    expectType varType expr ("right hand side of assignment to " ++ name)

checkStmt (AbsLatte.Incr var @ (Ident name)) =
    expectType TcInt (EVar var TcNone) ("variable " ++ name)
checkStmt (AbsLatte.Decr var @ (Ident name)) =
    expectType TcInt (EVar var TcNone) ("variable " ++ name)
checkStmt (Ret expr) = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (TcFun retType _) = currentFunction
    expectType retType expr "return value"
checkStmt VRet = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (TcFun retType _) = currentFunction
    when (retType /= TcVoid) $
        reportError ("function " ++ currentFunctionName ++ " has to return value")
checkStmt (Cond expr stmt) = do
    expectType TcBool expr "if condition"
    checkStmt stmt
checkStmt (CondElse expr stmtIf stmtElse) = do
    expectType TcBool expr "if condition"
    checkStmt stmtIf
    checkStmt stmtElse
checkStmt (While expr stmt) = do
    expectType TcBool expr "while condition"
    checkStmt stmt
checkStmt (SExp expr) = checkExpr expr >> return ()

checkDecl :: TcType -> Item -> Check ()
checkDecl varType (NoInit (Ident name)) = addVariable name varType
checkDecl varType (Init (Ident name) expr) = do
    addVariable name varType
    checkStmt (Ass (Ident name) expr)

checkExpr :: Expr -> Check TcType
checkExpr (EVar (Ident name) _) = getVariableType name
checkExpr (ELitInt _ _) = return TcInt
checkExpr (ELitTrue _) = return TcBool
checkExpr (ELitFalse _) = return TcBool
checkExpr (EApp (Ident funName) args _) = do
    funType <- getFunctionType funName
    case funType of
        TcFun retType funArgTypes -> do
            when ((length args) /= (length funArgTypes)) $
                reportError ("Incorrect number of arguments for function " ++ funName)
            argTypes <- forM args checkExpr
            when (argTypes /= funArgTypes) $ reportError
                ("Argument types for function " ++ funName ++ " does not match")
            return retType
checkExpr (EString string _) = return TcStr
checkExpr (Neg expr _) = do
    expectType TcInt expr "negation argument"
    return TcInt
checkExpr (Not expr _) = do
    expectType TcBool expr "negation argument"
    return TcBool
checkExpr (EMul lhs mulop rhs _) = do
    expectType TcInt lhs "left multiplication operand"
    expectType TcInt rhs "right multiplication operand"
    return TcInt
checkExpr (EAdd lhs addop rhs _) = do
    expectType TcInt lhs "left addition operand"
    expectType TcInt rhs "right addition operand"
    return TcInt
checkExpr (ERel lhs relop rhs _) = do
    expectType TcInt lhs "left compare operand"
    expectType TcInt rhs "right compare operand"
    return TcBool
checkExpr (EAnd lhs rhs _) = do
    expectType TcBool lhs "left and operand"
    expectType TcBool rhs "right and operand"
    return TcBool
checkExpr (EOr lhs rhs _) = do
    expectType TcBool lhs "left and operand"
    expectType TcBool rhs "right and operand"
    return TcBool

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

expectType :: TcType -> Expr -> String -> Check ()
expectType expectedType expr what = do
    actualType <- checkExpr expr
    when (expectedType /= actualType) $ reportError ("Incorrect type of " ++ what ++
        ": expected " ++ (show expectedType) ++ ", got " ++ (show actualType))
    return ()

typeToTcType :: Type -> TcType
typeToTcType Int  = TcInt
typeToTcType Str  = TcStr
typeToTcType Bool = TcBool
typeToTcType Void = TcVoid

reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
