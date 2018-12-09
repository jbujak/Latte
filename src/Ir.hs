module Ir ( Ir,
            IrFunction(name, commands, locals),
            IrCommand(LoadConst, Call, Return),
            IrConst(ConstInt, ConstString),
            Local,
            generateIr) where

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List


-- Types definition

type Ir = [IrFunction]
type Local = Integer

data IrFunction = Function {
    name :: String,
    commands :: [IrCommand],
    locals :: Integer
} deriving (Show)

data IrCommand =
    LoadConst Local IrConst
    | Call Local String [Local]
    | Return (Maybe Local)
  deriving Show

data IrConst =
    ConstInt Integer
    | ConstString Integer
  deriving Show

data GenerateState = State {
    functions :: [IrFunction],
    currentFunction :: Maybe IrFunction
}

type Generate a = (StateT GenerateState (Either String)) a


-- Monad boilerplate

generateIr :: Program -> Either String [IrFunction]
generateIr prog = runGeneration $ generateProgram prog

runGeneration :: Generate () -> Either String [IrFunction]
runGeneration m = fmap (functions . snd) $ runStateT m emptyGenerateState

emptyGenerateState :: GenerateState
emptyGenerateState = State {
    functions = [],
    currentFunction = Nothing
}


-- IR generator implementation

generateProgram :: Program -> Generate ()
generateProgram (Prog topdefs) = forM_ topdefs generateTopDef

generateTopDef :: TopDef -> Generate ()
generateTopDef (FnDef returnType (Ident name) args (Blk stmts)) = do
    startFunction name
    forM_ stmts generateStmt
    endFunction

generateStmt :: Stmt -> Generate ()
generateStmt Empty = return ()
generateStmt (BStmt (Blk stmts)) = forM_ stmts generateStmt
generateStmt (Decl declType items) = reportError "Not yet implemented: Decl"
generateStmt (Ass ident expr) = reportError "Not yet implemented: Ass"
generateStmt (Incr ident) = reportError "Not yet implemented: Incr"
generateStmt (Decr ident) = reportError "Not yet implemented: Decr"
generateStmt (Ret expr) = do
    local <- generateExpr expr
    printCommand (Return $ Just local)

generateStmt VRet = reportError "Not yet implemented: VRet"
generateStmt (Cond expr stmt) = reportError "Not yet implemented: Cond"
generateStmt (CondElse expr stmtIf stmtElse) = reportError "Not yet implemented: CondElse"
generateStmt (While sxpr stmt) = reportError "Not yet implemented: While"
generateStmt (SExp expr) = do
    generateExpr expr
    return ()

generateExpr :: Expr -> Generate Local
generateExpr (EVar ident) = do
    reportError "Not yet implemented: EVar"
    return 1
generateExpr (ELitInt integer) = do
    local <- newLocal
    printCommand $ LoadConst local (ConstInt integer)
    return local
generateExpr (ELitTrue) = do
    local <- newLocal
    printCommand $ LoadConst local (ConstInt 1)
    return local
generateExpr (ELitFalse) = do
    local <- newLocal
    printCommand $ LoadConst local (ConstInt 0)
    return local
generateExpr (EApp (Ident funName) expr) = do
    argsLocals <- forM expr generateExpr
    local <- newLocal
    printCommand $ Call local funName argsLocals
    return local
generateExpr (EString string) = do
    reportError "Not yet implemented: EString"
    return 1
generateExpr (Neg expr) = do
    reportError "Not yet implemented: Neg"
    return 1
generateExpr (Not expr) = do
    reportError "Not yet implemented: Not"
    return 1
generateExpr (EMul lhs mulop rhs) = do
    reportError "Not yet implemented: EMul"
    return 1
generateExpr (EAdd lhs addop rhs) = do
    reportError "Not yet implemented: EAdd"
    return 1
generateExpr (ERel lhs relop rhs) = do
    reportError "Not yet implemented: ERel"
    return 1
generateExpr (EAnd lhs rhs) = do
    reportError "Not yet implemented: EAnd"
    return 1
generateExpr (EOr lhs rhs) = do
    reportError "Not yet implemented: "
    return 1


-- Auxiliary functions

startFunction :: String -> Generate ()
startFunction name = do
    modify $ \s -> s { currentFunction = Just (Function {
        name = name,
        commands = [],
        locals = 0
    })}

printCommand :: IrCommand -> Generate ()
printCommand cmd = do
    currentFunction <- getCurrentFunction
    modify $ \s -> s {
        currentFunction = Just currentFunction {
            -- printCommand adds command to the begining of list for performance
            -- this will cause commands to be in reverse order while building function
            commands = cmd:(commands currentFunction)
        }
    }
    return ()

endFunction :: Generate ()
endFunction = do
    currentFunction <- getCurrentFunction
    -- reverse commands list to restore natural order
    let insertedFunction = currentFunction {
        commands = reverse $ commands currentFunction
    }
    functions <- gets functions
    modify $ \s -> s {
        functions = insertedFunction:functions,
        currentFunction = Nothing
    }

newLocal :: Generate Local
newLocal = do
    function <- getCurrentFunction
    let newLocals = locals function + 1
    modify $ \s -> s {
        currentFunction = Just (function {
            locals = newLocals
        })
    }
    return newLocals

getCurrentFunction :: Generate IrFunction
getCurrentFunction = do
    maybeFunction <- gets currentFunction
    case maybeFunction of
        Just currentFunction -> return currentFunction
        Nothing -> reportError "No current function"


reportError :: String -> Generate a
reportError msg = StateT { runStateT = \s -> Left msg }
