module TypeChecker (checkTypes) where

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List

data CheckState = State {
    functions :: [(String, Type)],
    currentFunction :: String,
    variables :: [(String, Type)]
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


-- Type checker implementation

checkProgram :: Program -> Check ()
checkProgram (Prog topdefs) = do
    forM_ topdefs addTopDef
    forM_ topdefs checkTopDef

addTopDef :: TopDef -> Check ()
addTopDef (FnDef returnType (Ident name) args _) =
    let argsTypes = map (\(Ar argType _) -> argType) args in
    addFunction name (Fun returnType argsTypes)

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
checkStmt (Decl varType items) = forM_ items (checkDecl varType)
checkStmt (Ass (Ident name) expr) = do
    varType <- getVariableType name
    exprType <- checkExpr expr
    when (varType /= exprType) $ reportError ("Incorrect type of variable " ++ name ++
        ": exprected " ++ show exprType ++ ", got " ++ show varType)

checkStmt (AbsLatte.Incr (Ident name)) = return () --TODO
checkStmt (AbsLatte.Decr (Ident name)) = return () --TODO
checkStmt (Ret expr) = return () --TODO
checkStmt VRet = return () --TODO
checkStmt (Cond expr stmt) = return () --TODO
checkStmt (CondElse expr stmtIf stmtElse) = return () --TODO
checkStmt (While expr stmt) = return () --TODO
checkStmt (SExp expr) = return () --TODO

checkDecl :: Type -> Item -> Check ()
checkDecl varType (NoInit (Ident name)) = addVariable name varType
checkDecl varType (Init (Ident name) expr) = do
    addVariable name varType
    checkStmt (Ass (Ident name) expr)

checkExpr :: Expr -> Check Type
checkExpr (EVar (Ident name)) = getVariableType name
checkExpr (ELitInt _) = return Int
checkExpr (ELitTrue) = return Bool
checkExpr (ELitFalse) = return Bool
checkExpr (EApp (Ident funName) args) = do
    funType <- getFunctionType funName
    case funType of
        -- TODO check args
        Fun retType _ -> return retType
        _             -> reportError "Internal compiler error"
checkExpr (EString string) = return Str
checkExpr (Neg expr) = do
    exprType <- checkExpr expr
    when (exprType /= Bool) $ reportError "Expected type: boolean"
    return Bool
checkExpr (Not expr) = do
    exprType <- checkExpr expr
    when (exprType /= Bool) $ reportError "Expected type: boolean"
    return Bool
checkExpr (EMul lhs mulop rhs) = do
    lhsType <- checkExpr lhs
    rhsType <- checkExpr rhs
    when (lhsType /= Int || rhsType /= Int) $ reportError "Expected type: int"
    return Int
checkExpr (EAdd lhs addop rhs) = do
    lhsType <- checkExpr lhs
    rhsType <- checkExpr rhs
    when (lhsType /= Int || rhsType /= Int) $ reportError "Expected type: int"
    return Int
checkExpr (ERel lhs relop rhs) = do
    lhsType <- checkExpr lhs
    rhsType <- checkExpr rhs
    when (lhsType /= Int || rhsType /= Int) $ reportError "Expected type: int"
    return Int
checkExpr (EAnd lhs rhs) = do
    lhsType <- checkExpr lhs
    rhsType <- checkExpr rhs
    when (lhsType /= Bool || rhsType /= Bool) $ reportError "Expected type: boolean"
    return Bool
checkExpr (EOr lhs rhs) = do
    lhsType <- checkExpr lhs
    rhsType <- checkExpr rhs
    when (lhsType /= Bool || rhsType /= Bool) $ reportError "Expected type: boolean"
    return Bool


beginFunction :: String -> Check ()
beginFunction name = modify $ \s -> s { currentFunction = name }

-- Auxiliary functions

addFunction :: String -> Type -> Check ()
addFunction name funType =  do
    functions <- gets functions
    modify $ \s -> s {
        functions = (name, funType):functions
    }

getFunctionType :: String -> Check Type
getFunctionType name = do
    case getBuiltinFunctionType name of
        Just funType -> return funType
        Nothing -> do
            function <- gets functions
            case lookup name function of
                Just funType -> return funType
                Nothing      -> reportError $ "Unkown function " ++ name

getBuiltinFunctionType :: String -> Maybe Type
getBuiltinFunctionType "printInt" = return $ Fun Void [Int]
getBuiltinFunctionType "printString" = return $ Fun Void [Str]
getBuiltinFunctionType "readInt" = return $ Fun Int []
getBuiltinFunctionType "error" = return $ Fun Void []

addArg :: Arg -> Check ()
addArg (Ar argType (Ident name)) = addVariable name argType

addVariable :: String -> Type -> Check ()
addVariable name varType =  do
    variables <- gets variables
    modify $ \s -> s {
        variables = (name, varType):variables
    }

getVariableType :: String -> Check Type
getVariableType name = do
    variables <- gets variables
    case lookup name variables of
        Just varType -> return varType
        Nothing      -> reportError $ "Unkown variable " ++ name

reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
