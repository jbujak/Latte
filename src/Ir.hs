module Ir ( Ir, generateIr) where

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List


-- Types definition

type Ir = [IrFunction]
type Reg = Integer

data IrFunction = Function {
    name :: String,
    commands :: [IrCommand],
    registers :: Integer
} deriving (Show)

data IrCommand =
    Nop
    | LoadConst Reg IrConst
    | Call Reg String [Reg]
    | Return (Maybe Reg)
  deriving Show

data IrConst =
    ConstInt Integer
    | ConstBool Bool
    | ConstString String
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


-- Ir generator implementation

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
    reg <- generateExpr expr
    printCommand (Return $ Just reg)

generateStmt VRet = reportError "Not yet implemented: VRet"
generateStmt (Cond expr stmt) = reportError "Not yet implemented: Cond"
generateStmt (CondElse expr stmtIf stmtElse) = reportError "Not yet implemented: CondElse"
generateStmt (While sxpr stmt) = reportError "Not yet implemented: While"
generateStmt (SExp expr) = do
    generateExpr expr
    return ()

generateExpr :: Expr -> Generate Reg
generateExpr (EVar ident) = do
    reportError "Not yet implemented: EVar"
    return 1
generateExpr (ELitInt integer) = do
    reg <- newReg
    printCommand $ LoadConst reg (ConstInt integer)
    return reg
generateExpr (ELitTrue) = do
    reg <- newReg
    printCommand $ LoadConst reg (ConstBool True)
    return reg
generateExpr (ELitFalse) = do
    reg <- newReg
    printCommand $ LoadConst reg (ConstBool False)
    return reg
generateExpr (EApp (Ident funName) expr) = do
    argsRegisters <- forM expr generateExpr
    reg <- newReg
    printCommand $ Call reg funName argsRegisters
    return reg
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
        registers = 0
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

newReg :: Generate Reg
newReg = do
    function <- getCurrentFunction
    let new_registers = registers function + 1
    modify $ \s -> s {
        currentFunction = Just (function {
            registers = new_registers
        })
    }
    return new_registers

getCurrentFunction :: Generate IrFunction
getCurrentFunction = do
    maybeFunction <- gets currentFunction
    case maybeFunction of
        Just currentFunction -> return currentFunction
        Nothing -> reportError "No current function"


reportError :: String -> Generate a
reportError msg = StateT { runStateT = \s -> Left msg }
