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
checkStmt (Decl varType items) = return () --TODO
checkStmt (Ass (Ident name) expr) = return () --TODO
checkStmt (AbsLatte.Incr (Ident name)) = return () --TODO
checkStmt (AbsLatte.Decr (Ident name)) = return () --TODO
checkStmt (Ret expr) = return () --TODO
checkStmt VRet = return () --TODO
checkStmt (Cond expr stmt) = return () --TODO
checkStmt (CondElse expr stmtIf stmtElse) = return () --TODO
checkStmt (While expr stmt) = return () --TODO
checkStmt (SExp expr) = return () --TODO

beginFunction :: String -> Check ()
beginFunction name = modify $ \s -> s { currentFunction = name }

-- Auxiliary functions

addFunction :: String -> Type -> Check ()
addFunction name funType =  do
    functions <- gets functions
    modify $ \s -> s {
        functions = (name, funType):functions
    }

addArg :: Arg -> Check ()
addArg (Ar argType (Ident name)) = addVariable name argType

addVariable :: String -> Type -> Check ()
addVariable name varType =  do
    variables <- gets variables
    modify $ \s -> s {
        variables = (name, varType):variables
    }

reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
