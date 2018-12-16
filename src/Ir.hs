module Ir ( Ir,
            IrFunction(name, commands, locals),
            IrCommand(..),
            IrConst(..),
            BinOpType(..),
            Local, Label,
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
type Label = Integer

data IrFunction = Function {
    name :: String,
    commands :: [IrCommand],
    locals :: Integer
} deriving (Show)

data IrCommand =
    LoadConst Local IrConst
    | Nop
    | Call Local String [Local]
    | Return (Maybe Local)
    | BinOp Local Local Local BinOpType
    | Goto Label
    | GotoIf Local Label
    | PrintLabel Label
  deriving Show

data IrConst =
    ConstInt Integer
    | ConstString Integer
  deriving Show

data BinOpType = Add | Sub | Mul | Div | Mod | Lt | Lte | Gt | Gte | Eq | Neq deriving Show

data GenerateState = State {
    functions :: [IrFunction],
    currentFunction :: Maybe IrFunction,
    nextLabel :: Label
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
    currentFunction = Nothing,
    nextLabel = 0
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
generateStmt Empty = printCommand Nop
generateStmt (BStmt (Blk stmts)) = forM_ stmts generateStmt
generateStmt (Decl declType items) = reportError "Not yet implemented: Decl"
generateStmt (Ass ident expr) = reportError "Not yet implemented: Ass"
generateStmt (Incr ident) = reportError "Not yet implemented: Incr"
generateStmt (Decr ident) = reportError "Not yet implemented: Decr"
generateStmt (Ret expr) = do
    local <- generateExpr expr
    printCommand (Return $ Just local)

generateStmt VRet = reportError "Not yet implemented: VRet"
generateStmt (Cond expr stmt) = generateStmt (CondElse expr stmt Empty)
generateStmt (CondElse expr stmtIf stmtElse) = do
    localCond  <- generateExpr expr
    labelIf    <- newLabel
    labelEnd <- newLabel
    printCommand $ GotoIf localCond labelIf
    generateStmt stmtElse
    printCommand $ Goto labelEnd
    printCommand $ PrintLabel labelIf
    generateStmt stmtIf
    printCommand $ PrintLabel labelEnd
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
generateExpr (Neg expr) = generateBinOpExpr (ELitInt $ -1) expr Ir.Mul
generateExpr (Not expr) = do
    reportError "Not yet implemented: Not"
    return 1
generateExpr (EMul lhs mulop rhs) = do
    generateBinOpExpr lhs rhs (mulOpToBinOp mulop)
generateExpr (EAdd lhs addop rhs) = do
    generateBinOpExpr lhs rhs (addOpToBinOp addop)
generateExpr (ERel lhs relop rhs) = do
    generateBinOpExpr lhs rhs (relOpToBinOp relop)
generateExpr (EAnd lhs rhs) = do
    reportError "Not yet implemented: EAnd"
    return 1
generateExpr (EOr lhs rhs) = do
    reportError "Not yet implemented: "
    return 1

generateBinOpExpr :: Expr -> Expr -> BinOpType -> Generate (Local)
generateBinOpExpr lhs rhs binOp = do
    lhsLocal <- generateExpr lhs
    rhsLocal <- generateExpr rhs
    resLocal <- newLocal
    printCommand $ BinOp resLocal lhsLocal rhsLocal binOp
    return resLocal


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

newLabel :: Generate Label
newLabel = do
    label <- gets nextLabel
    modify $ \s -> s {
        nextLabel = label + 1
    }
    return label

getCurrentFunction :: Generate IrFunction
getCurrentFunction = do
    maybeFunction <- gets currentFunction
    case maybeFunction of
        Just currentFunction -> return currentFunction
        Nothing -> reportError "No current function"

mulOpToBinOp :: MulOp -> BinOpType
mulOpToBinOp AbsLatte.Times = Ir.Mul
mulOpToBinOp AbsLatte.Div   = Ir.Div
mulOpToBinOp AbsLatte.Mod   = Ir.Mod

addOpToBinOp :: AddOp -> BinOpType
addOpToBinOp Plus  = Add
addOpToBinOp Minus = Sub

relOpToBinOp :: RelOp -> BinOpType
relOpToBinOp LTH = Lt
relOpToBinOp LE  = Lte
relOpToBinOp GTH = Gt
relOpToBinOp GE  = Gte
relOpToBinOp EQU = Eq
relOpToBinOp NE  = Neq

reportError :: String -> Generate a
reportError msg = StateT { runStateT = \s -> Left msg }
