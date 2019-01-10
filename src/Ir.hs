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
    | LoadArgs [Local]
    | Return (Maybe Local)
    | BinOp Local Local Local BinOpType
    | Goto Label
    | GotoIf Local Label
    | PrintLabel Label
    | Assign Local Local
    | ArrCreate Local Local
    | ArrGet Local Local Local
    | ArrSet Local Local Local
  deriving Show

data IrConst =
      ConstInt Integer
    | ConstString Integer
  deriving Show

data UnOpType  = Incr | Decr

data BinOpType = Add | Sub | Mul | Div | Mod |
                 Lt  | Lte | Gt  | Gte | Eq  | Neq |
                 And | Or  | Xor |
                 Concat
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
generateStmt (BStmt (Blk stmts)) = do
    currentFunctionBefore <- getCurrentFunction
    let variablesBefore = variables currentFunctionBefore
    forM_ stmts generateStmt
    currentFunctionAfter <- getCurrentFunction
    modify $ \s -> s {
        currentFunction = Just $ currentFunctionAfter { variables = variablesBefore }
    }
generateStmt (Decl _ items) = forM_ items generateItem
generateStmt (Ass (Var (Ident name) _) expr) = do
    local <- getVariable name
    result <- generateExpr expr
    printCommand $ Assign local result
generateStmt (Ass (ArrElem (Ident name) indexExpr _) expr) = do
    arrLocal   <- getVariable name
    indexLocal <- generateExpr indexExpr
    exprLocal  <- generateExpr expr
    printCommand $ ArrSet arrLocal indexLocal exprLocal
generateStmt (AbsLatte.Incr lval) = generateUnOpExpr lval Ir.Incr
generateStmt (AbsLatte.Decr lval) = generateUnOpExpr lval Ir.Decr
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
    breakWhileCond <- generateExpr (Not expr TcBool)
    printCommand $ GotoIf breakWhileCond endLabel
    generateStmt stmt
    printCommand $ Goto condLabel
    printCommand $ PrintLabel endLabel
generateStmt (SExp expr) = do
    generateExpr expr
    return ()

generateExpr :: Expr -> Generate Local
generateExpr (EArrNew _ sizeExpr _) = do
    sizeLocal <- generateExpr sizeExpr
    dstLocal  <- newLocal
    printCommand $ ArrCreate dstLocal sizeLocal
    return dstLocal
generateExpr (ELVal (Var (Ident name) _) _) = do
    dstLocal <- newLocal
    srcLocal <- getVariable name
    printCommand $ Assign dstLocal srcLocal
    return dstLocal
generateExpr (ELVal (ArrElem (Ident name) indexExpr _) _) = do
    dstLocal  <- newLocal
    arrLocal  <- getVariable name
    indexLocal <- generateExpr indexExpr
    printCommand $ ArrGet dstLocal arrLocal indexLocal
    return dstLocal
generateExpr (ELitInt integer _) = do
    local <- newLocal
    printCommand $ LoadConst local (ConstInt integer)
    return local
generateExpr (ELitTrue _) = do
    local <- newLocal
    printCommand $ LoadConst local (ConstInt 1)
    return local
generateExpr (ELitFalse _) = do
    local <- newLocal
    printCommand $ LoadConst local (ConstInt 0)
    return local
generateExpr (EApp (Ident funName) expr _) = do
    argsLocals <- forM expr generateExpr
    local <- newLocal
    printCommand $ Call local funName argsLocals
    return local
generateExpr (EString string _) = newString string
generateExpr (Neg expr _) = generateBinOpExpr (ELitInt (-1) TcInt) expr Ir.Mul
generateExpr (Not expr _) = do
    exprLocal <- generateExpr expr
    resultLocal  <- newLocal
    one <- generateExpr $ ELitInt 1 TcInt
    printCommand $ BinOp resultLocal exprLocal one Xor
    return resultLocal
generateExpr (EMul lhs mulop rhs _) = do
    generateBinOpExpr lhs rhs (mulOpToBinOp mulop)
generateExpr (EAdd lhs addop rhs TcInt) = do
    generateBinOpExpr lhs rhs (addOpToBinOp addop)
generateExpr (EAdd lhs addop rhs TcStr) = do
    generateBinOpExpr lhs rhs Concat
generateExpr (ERel lhs relop rhs _) = do
    generateBinOpExpr lhs rhs (relOpToBinOp relop)
generateExpr (EAnd lhs rhs _) = do
    generateBinOpExpr lhs rhs Ir.And
generateExpr (EOr lhs rhs _) = do
    generateBinOpExpr lhs rhs Ir.Or

generateItem :: Item -> Generate ()
generateItem (NoInit (Ident name)) = do
    newVariable name
    return ()
generateItem (Init (Ident name) expr) = do
    result   <- generateExpr expr
    varLocal <- newVariable name
    printCommand $ Assign varLocal result

generateArgs :: [Arg] -> Generate ()
generateArgs args = do
    modify $ \s -> s { loadedArgs = 0 }
    args <- forM args generateArg
    printCommand $ LoadArgs args

generateArg :: Arg -> Generate Local
generateArg (Ar _ (Ident name)) = newVariable name

generateUnOpExpr :: LVal -> UnOpType -> Generate ()
generateUnOpExpr lval op = do
    let binOp = case op of
            Ir.Incr -> Plus
            Ir.Decr -> Minus
    let lvalExpr = ELVal lval (getLValType lval)
    let oneExpr = ELitInt 1 TcInt
    let binOpExpr = EAdd lvalExpr binOp oneExpr TcInt
    generateStmt $ Ass lval binOpExpr

generateBinOpExpr :: Expr -> Expr -> BinOpType -> Generate (Local)
generateBinOpExpr lhs rhs And = do
    resLocal <- newLocal
    finish   <- newLabel
    lhsLocal <- generateExpr lhs
    lhsNeg   <- newLocal
    one      <- generateExpr $ ELitInt 1 TcInt
    printCommand $ BinOp lhsNeg lhsLocal one Xor
    printCommand $ Assign resLocal lhsLocal
    printCommand $ GotoIf lhsNeg finish
    rhsLocal <- generateExpr rhs
    printCommand $ BinOp resLocal lhsLocal rhsLocal And
    printCommand $ PrintLabel finish
    return resLocal
generateBinOpExpr lhs rhs Or = do
    resLocal <- newLocal
    finish   <- newLabel
    lhsLocal <- generateExpr lhs
    printCommand $ Assign resLocal lhsLocal
    printCommand $ GotoIf lhsLocal finish
    rhsLocal <- generateExpr rhs
    printCommand $ BinOp resLocal lhsLocal rhsLocal Or
    printCommand $ PrintLabel finish
    return resLocal
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

getLValType :: LVal -> TcType
getLValType (Var _ tcType) = tcType
getLValType (ArrElem _ _ tcType) = tcType

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
