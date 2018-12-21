module Ir ( Ir(..),
            IrFunction(..),
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

data Ir =  Ir {
    irFunctions :: [IrFunction],
    stringLiterals :: [String]
} deriving Show

type Local = Integer
type Label = Integer

data IrFunction = Function {
    name :: String,
    commands :: [IrCommand],
    locals :: Integer,
    variables :: [(String, Local)]
} deriving Show

data IrCommand =
      Nop
    | LoadConst Local IrConst
    | Call Local String [Local]
    | LoadArg Local Integer
    | Return (Maybe Local)
    | BinOp Local Local Local BinOpType
    | Goto Label
    | GotoIf Local Label
    | PrintLabel Label
    | Assign Local Local
  deriving Show

data IrConst =
      ConstInt Integer
    | ConstString Integer
  deriving Show

data UnOpType  = Incr | Decr

data BinOpType = Add | Sub | Mul | Div | Mod |
                 Lt  | Lte | Gt  | Gte | Eq  | Neq |
                 And | Or  | Xor
               deriving Show

data GenerateState = State {
    functions :: [IrFunction],
    currentFunction :: Maybe IrFunction,
    nextLabel :: Label,
    loadedArgs :: Integer,
    strings :: [String]
}

type Generate a = (StateT GenerateState (Either String)) a


-- Monad boilerplate

generateIr :: Program -> Either String Ir
generateIr prog = runGeneration $ generateProgram prog

runGeneration :: Generate () -> Either String Ir
runGeneration m = fmap (createIr . snd) $ runStateT m emptyGenerateState

createIr :: GenerateState -> Ir
createIr s = Ir {
    irFunctions = functions s,
    stringLiterals = strings s
}

emptyGenerateState :: GenerateState
emptyGenerateState = State {
    functions = [],
    currentFunction = Nothing,
    nextLabel = 0,
    loadedArgs = 0,
    strings = []
}


-- IR generator implementation

generateProgram :: Program -> Generate ()
generateProgram (Prog topdefs) = forM_ topdefs generateTopDef

generateTopDef :: TopDef -> Generate ()
generateTopDef (FnDef returnType (Ident name) args (Blk stmts)) = do
    startFunction name
    generateArgs args
    forM_ stmts generateStmt
    endFunction

generateStmt :: Stmt -> Generate ()
generateStmt Empty = printCommand Nop
generateStmt (BStmt (Blk stmts)) = forM_ stmts generateStmt
generateStmt (Decl _ items) = forM_ items generateItem
generateStmt (Ass (Ident name) expr) = do
    local <- getVariable name
    result <- generateExpr expr
    printCommand $ Assign local result
generateStmt (AbsLatte.Incr (Ident name)) = generateUnOpExpr name Ir.Incr
generateStmt (AbsLatte.Decr (Ident name)) = generateUnOpExpr name Ir.Decr
generateStmt (Ret expr) = do
    local <- generateExpr expr
    printCommand (Return $ Just local)
generateStmt VRet = printCommand $ Return Nothing
generateStmt (Cond expr stmt) = generateStmt (CondElse expr stmt Empty)
generateStmt (CondElse expr stmtIf stmtElse) = do
    localCond  <- generateExpr expr
    labelIf    <- newLabel
    labelEnd   <- newLabel
    printCommand $ GotoIf localCond labelIf
    generateStmt stmtElse
    printCommand $ Goto labelEnd
    printCommand $ PrintLabel labelIf
    generateStmt stmtIf
    printCommand $ PrintLabel labelEnd
generateStmt (While expr stmt) = do
    condLabel <- newLabel
    endLabel  <- newLabel
    printCommand $ PrintLabel condLabel
    breakWhileCond <- generateExpr (Not expr)
    printCommand $ GotoIf breakWhileCond endLabel
    generateStmt stmt
    printCommand $ Goto condLabel
    printCommand $ PrintLabel endLabel
generateStmt (SExp expr) = do
    generateExpr expr
    return ()

generateExpr :: Expr -> Generate Local
generateExpr (EVar (Ident name)) = do
    dstLocal <- newLocal
    srcLocal <- getVariable name
    printCommand $ Assign dstLocal srcLocal
    return dstLocal
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
generateExpr (EString string) = newString string
generateExpr (Neg expr) = generateBinOpExpr (ELitInt $ -1) expr Ir.Mul
generateExpr (Not expr) = do
    exprLocal <- generateExpr expr
    resultLocal  <- newLocal
    one <- generateExpr $ ELitInt 1
    printCommand $ BinOp resultLocal exprLocal one Xor
    return resultLocal
generateExpr (EMul lhs mulop rhs) = do
    generateBinOpExpr lhs rhs (mulOpToBinOp mulop)
generateExpr (EAdd lhs addop rhs) = do
    generateBinOpExpr lhs rhs (addOpToBinOp addop)
generateExpr (ERel lhs relop rhs) = do
    generateBinOpExpr lhs rhs (relOpToBinOp relop)
generateExpr (EAnd lhs rhs) = do
    generateBinOpExpr lhs rhs Ir.And
generateExpr (EOr lhs rhs) = do
    generateBinOpExpr lhs rhs Ir.Or

generateItem :: Item -> Generate ()
generateItem (NoInit (Ident name)) = do
    newVariable name
    return ()
generateItem (Init (Ident name) expr) = do
    varLocal <- newVariable name
    result   <- generateExpr expr
    printCommand $ Assign varLocal result

generateArgs :: [Arg] -> Generate ()
generateArgs args = do
    modify $ \s -> s { loadedArgs = 0 }
    forM_ args generateArg

generateArg :: Arg -> Generate ()
generateArg (Ar _ (Ident name)) = do
    local <- newVariable name
    argNo <- gets loadedArgs
    modify $ \s -> s { loadedArgs = argNo + 1 }
    printCommand $ LoadArg local argNo


generateUnOpExpr :: String -> UnOpType -> Generate ()
generateUnOpExpr name op = do
    local <- getVariable name
    let binOp = case op of
            Ir.Incr -> Ir.Add
            Ir.Decr -> Ir.Sub
    one <- generateExpr $ ELitInt 1
    printCommand $ BinOp local local one binOp

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
        locals = 0,
        variables = []
    })}

-- printCommand adds command to the begining of list for performance reasons
-- this will cause commands to be in reverse order during building function
printCommand :: IrCommand -> Generate ()
printCommand cmd = do
    currentFunction <- getCurrentFunction
    modify $ \s -> s {
        currentFunction = Just currentFunction {
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

newVariable :: String -> Generate Local
newVariable name = do
    local <- newLocal
    function <- getCurrentFunction
    modify $ \s -> s {
        currentFunction = Just (function {
            variables = (name, local):(variables function)
        })
    }
    return local

newString :: String -> Generate Integer
newString str = do
    local <- newLocal
    strings <- gets strings
    modify $ \s -> s {
        strings = str:strings
    }
    printCommand $ LoadConst local (ConstString (toInteger $ length strings))
    return local

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

getVariable :: String -> Generate Local
getVariable name = do
    function <- getCurrentFunction
    case lookup name $ variables function of
        Nothing    -> reportError "Unknown variable"
        Just local -> return local

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

reportError :: err -> StateT a (Either err) b
reportError msg = StateT { runStateT = \s -> Left msg }
