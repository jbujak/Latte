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
    expectType varType expr ("right hand side of assignment to " ++ name)

checkStmt (AbsLatte.Incr (Ident name)) = return () --TODO
checkStmt (AbsLatte.Decr (Ident name)) = return () --TODO
checkStmt (Ret expr) = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (Fun retType _) = currentFunction
    expectType retType expr "return value"
checkStmt VRet = do
    currentFunctionName <- gets currentFunction
    currentFunction <- getFunctionType currentFunctionName
    let (Fun retType _) = currentFunction
    when (retType /= Void) $
        reportError ("function " ++ currentFunctionName ++ " has to return value")
checkStmt (Cond expr stmt) = return () --TODO
checkStmt (CondElse expr stmtIf stmtElse) = return () --TODO
checkStmt (While expr stmt) = return () --TODO
checkStmt (SExp expr) = checkExpr expr >> return ()

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
checkExpr (EString string) = return Str
checkExpr (Neg expr) = do
    expectType Int expr "negation argument"
    return Int
checkExpr (Not expr) = do
    expectType Bool expr "negation argument"
    return Bool
checkExpr (EMul lhs mulop rhs) = do
    expectType Int lhs "left multiplication operand"
    expectType Int rhs "right multiplication operand"
    return Int
checkExpr (EAdd lhs addop rhs) = do
    expectType Int lhs "left addition operand"
    expectType Int rhs "right addition operand"
    return Int
checkExpr (ERel lhs relop rhs) = do
    expectType Int lhs "left compare operand"
    expectType Int rhs "right compare operand"
    return Bool
checkExpr (EAnd lhs rhs) = do
    expectType Bool lhs "left and operand"
    expectType Bool rhs "right and operand"
    return Bool
checkExpr (EOr lhs rhs) = do
    expectType Bool lhs "left and operand"
    expectType Bool rhs "right and operand"
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
getBuiltinFunctionType "printInt" = Just $ Fun Void [Int]
getBuiltinFunctionType "printString" = Just $ Fun Void [Str]
getBuiltinFunctionType "readInt" = Just $ Fun Int []
getBuiltinFunctionType "error" = Just $ Fun Void []
getBuiltinFunctionType _ = Nothing

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

expectType :: Type -> Expr -> String -> Check ()
expectType expectedType expr what = do
    actualType <- checkExpr expr
    when (expectedType /= actualType) $ reportError ("Incorrect type of " ++ what ++
        ": expected " ++ (show expectedType) ++ ", got " ++ (show actualType))
    return ()


reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
