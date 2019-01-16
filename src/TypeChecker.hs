module TypeChecker (checkTypes, Classes, ClassFields(..), ClassMethods(..)) where

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List

data CheckState = State {
    functions :: [(String, TcType)],
    currentFunctionName :: String,
    currentFunctionType :: TcType,
    variables :: [(String, TcType)],
    innerVariables :: [(String)],
    returned :: Bool,
    classes :: Classes
}

type Classes = [(String, (ClassFields, ClassMethods))]
data ClassFields = ClassFields [(String, TcType)] deriving Eq
data ClassMethods = ClassMethods [(String, TcType)] deriving Eq

type Check a = (StateT CheckState (Either String)) a

-- Monad boilerplate

checkTypes :: Program -> Either String (Program, Classes)
checkTypes prog = runChecker $ checkProgram prog

runChecker :: Check Program -> Either String (Program, Classes)
runChecker m = fmap (\s -> (fst s, classes $ snd s)) $ runStateT m emptyCheckState

emptyCheckState :: CheckState
emptyCheckState = State {
    functions = [],
    currentFunctionName = "",
    currentFunctionType = TcNone,
    variables = [],
    innerVariables = [],
    returned = False,
    classes = []
}


-- Type checker implementation

checkProgram :: Program -> Check Program
checkProgram (Prog topdefs) = do
    forM_ topdefs addTopDef
    typedTopDefs <- forM topdefs checkTopDef
    return $ Prog typedTopDefs

addTopDef :: TopDef -> Check ()
addTopDef (FnDef returnType (Ident name) args _) = do
    argsTypes    <- forM args (\(Ar argType _) -> (typeToTcType argType))
    returnTcType <- typeToTcType returnType
    addFunction name (TcFun returnTcType argsTypes)
addTopDef (ClassDef (Ident name) members) = do
    membersTypes  <- forM members memberToClassField
    let fieldsTypes  = filter isField membersTypes
    let methodsTypes = filter isMethod membersTypes
    addClass name (ClassFields fieldsTypes) (ClassMethods methodsTypes)
    return ()

memberToClassField :: ClassMember -> Check (String, TcType)
memberToClassField (Field fieldType (Ident fieldName)) = do
    tcType <- typeToTcType fieldType
    return (fieldName, tcType)
memberToClassField (Method returnType (Ident methodName) args block) = do
    argsTypes    <- forM args (\(Ar argType _) -> (typeToTcType argType))
    returnTcType <- typeToTcType returnType
    return (methodName, TcFun returnTcType argsTypes)

isField :: (String, TcType) -> Bool
isField (_, (TcFun _ _)) = False
isField (_, _) = True

isMethod :: (String, TcType) -> Bool
isMethod (_, (TcFun _ _ )) = True
isMethod (_, _) = False
    

checkTopDef :: TopDef -> Check TopDef
checkTopDef (FnDef returnType (Ident name) args block) = do
    beginFunction name
    forM_ args addArg
    typedBlock  <- checkBlock block
    funReturned <- gets returned
    when (not funReturned && returnType /= Void) $
        reportError "function may not return value"
    return $ FnDef returnType (Ident name) args typedBlock
checkTopDef (ClassDef (Ident name) members) = do
    typedMembers <- forM members $ checkMember name
    return $ ClassDef (Ident name) typedMembers

checkMember :: String -> ClassMember -> Check ClassMember
checkMember className (Method returnType (Ident methodName) args block) = do
    beginMethod className methodName
    forM_ args addArg
    typedBlock  <- checkBlock block
    methodReturned <- gets returned
    when (not methodReturned && returnType /= Void) $
        reportError "method may not return value"
    return (Method returnType (Ident methodName) args typedBlock)
checkMember _ member = return member

checkBlock :: Block -> Check Block
checkBlock (Blk stmts) = do
    vars <- gets variables
    innerVars <- gets innerVariables
    modify $ \s -> s { innerVariables = [] }
    typedStmts <- forM stmts checkStmt
    modify $ \s -> s { variables = vars, innerVariables = innerVars }
    return $ Blk typedStmts

checkStmt :: Stmt -> Check Stmt
checkStmt Empty = return Empty
checkStmt (BStmt block) = do
    typedBlock <- checkBlock block
    return $ BStmt typedBlock
checkStmt (Decl varType items) = do
    varTcType <- typeToTcType varType
    typedItems <- forM items $ checkDecl varTcType
    setReturned False
    return $ Decl varType typedItems
checkStmt (Ass lval expr) = do
    (typedLVal, lvalType) <- checkLVal lval True
    typedExpr <- expectType lvalType expr ("right hand side of assignment")
    setReturned False
    return $ Ass typedLVal typedExpr
checkStmt (AbsLatte.Incr lval) = do
    (typedLval, lvalType) <- checkLVal lval True
    when (lvalType /= TcInt) (reportError "incremented value must be int")
    setReturned False
    return $ AbsLatte.Incr typedLval
checkStmt (AbsLatte.Decr lval) = do
    (typedLval, lvalType) <- checkLVal lval True
    when (lvalType /= TcInt) (reportError "decremented value must be int")
    setReturned False
    return $ AbsLatte.Decr typedLval
checkStmt (Ret expr) = do
    currentFunctionType <- gets currentFunctionType
    let (TcFun retType _) = currentFunctionType
    typedExpr <- expectType retType expr "return value"
    setReturned True
    return $ Ret typedExpr
checkStmt VRet = do
    currentFunctionType <- gets currentFunctionType
    let (TcFun retType _) = currentFunctionType
    when (retType /= TcVoid) $
        reportError "function has to return value"
    setReturned False
    return VRet
checkStmt (Cond expr stmt) = do
    typedExpr <- expectType TcBool expr "if condition"
    setReturned False
    typedStmt <- checkStmt stmt
    returned  <- gets returned
    case exprEval expr of
        Just True -> setReturned returned
        _         -> setReturned False
    return $ Cond typedExpr typedStmt
checkStmt (CondElse expr stmtIf stmtElse) = do
    typedExpr     <- expectType TcBool expr "if condition"
    setReturned False
    typedStmtIf   <- checkStmt stmtIf
    ifReturned    <- gets returned
    setReturned False
    typedStmtElse <- checkStmt stmtElse
    elseReturned  <- gets returned
    case exprEval expr of
        Just True  -> setReturned ifReturned
        Just False -> setReturned elseReturned
        Nothing    -> setReturned (ifReturned && elseReturned)
    return $ CondElse typedExpr typedStmtIf typedStmtElse
checkStmt (While expr stmt) = do
    typedExpr <- expectType TcBool expr "while condition"
    typedStmt <- checkStmt stmt
    return $ While typedExpr typedStmt
checkStmt (SExp expr) = do
    (typedExpr, _) <- checkExpr expr
    setReturned False
    return $ SExp typedExpr
checkStmt (For iterType (Ident iterName) arrExpr stmt) = do
    (typedArrExpr, arrType) <- checkExpr arrExpr
    iterTcType <- typeToTcType iterType
    case arrType of
        TcArr innerType -> when (iterTcType /= innerType)
            (reportError "incorrect type of iterator")
        _ -> reportError "for loop can be used only for arrays"
    vars <- gets variables
    innerVars <- gets innerVariables
    modify $ \s -> s { innerVariables = [] }
    addVariable iterName iterTcType
    typedStmt <- checkStmt stmt
    modify $ \s -> s { variables = vars, innerVariables = innerVars }
    return (For iterType (Ident iterName) typedArrExpr typedStmt)


checkDecl :: TcType -> Item -> Check Item
checkDecl varType decl @ (NoInit (Ident name)) = do
    addVariable name varType
    return decl
checkDecl varType (Init (Ident name) expr) = do
    addVariable name varType
    typedExpr <- expectType varType expr ("initialization of variable " ++ name)
    return $ Init (Ident name) typedExpr

checkLVal :: LVal -> Bool -> Check (LVal, TcType)
checkLVal (ObjField lval (Ident fieldName) _) editable = do
    (typedLval, lvalType) <- checkLVal lval False
    innerType <- getFieldType lvalType fieldName
    when editable $ checkEditable lvalType fieldName
    return (ObjField typedLval (Ident fieldName) innerType, innerType)
checkLVal (Var (Ident name) _) _ = do
    varType <- getVariableType name
    return (Var (Ident name) varType, varType)
checkLVal (ArrElem lval indexExpr _) _ = do
    typedIndexExpr <- expectType TcInt indexExpr "array index"
    (typedLval, lvalType) <- checkLVal lval False
    let (TcArr innerType) = lvalType
    return (ArrElem typedLval typedIndexExpr innerType, innerType)

checkExpr :: Expr -> Check (Expr, TcType)
checkExpr (EObjNew objType _) = do
    tcType <- typeToTcType objType
    case tcType of
        TcClass name -> return (EObjNew objType tcType, tcType)
        _            -> reportError "type of object created by new has to be class"
checkExpr (EArrNew arrType size _) = do
    typedSize <- expectType TcInt size "new array length"
    arrTcType <- typeToTcType arrType
    let exprType = TcArr arrTcType
    return (EArrNew arrType typedSize exprType, exprType)
checkExpr (ELVal lval _) = do
    (typedLval, lvalType) <- checkLVal lval False
    return (ELVal typedLval lvalType, lvalType)
checkExpr (ELitNull nullType _) = do
    nullTcType <- typeToTcType nullType
    return (ELitNull nullType nullTcType, nullTcType)
checkExpr (ELitInt n _) = return (ELitInt n TcInt, TcInt)
checkExpr (ELitTrue _) = return (ELitTrue TcBool, TcBool)
checkExpr (ELitFalse _) = return (ELitFalse TcBool, TcBool)
checkExpr (EMethod lval (Ident methodName) args _) = do
    (typedLVal, lvalType) <- checkLVal lval False
    case lvalType of
        TcClass className -> do
            methodType <- getMethodType className methodName
            let fullName = className ++ "." ++ methodName
            let (TcFun retType methodArgTypes) = methodType
            when ((length args) /= (length methodArgTypes)) $
                reportError "incorrect number of arguments"
            checkedArgs <- forM args checkExpr
            let typedArgs = map fst checkedArgs
            let argTypes  = map snd checkedArgs
            when (argTypes /= methodArgTypes) $ reportError
                ("Argument types for method " ++ fullName ++ " does not match")
            return (EMethod typedLVal (Ident methodName) typedArgs retType,
                retType)
        _ -> reportError "methods can only be called for object"
checkExpr (EApp (Ident funName) args _) = do
    funType <- getFunctionType funName
    let (TcFun retType funArgTypes) = funType
    when ((length args) /= (length funArgTypes)) $
        reportError "incorrect number of arguments"
    checkedArgs <- forM args checkExpr
    let typedArgs = map fst checkedArgs
    let argTypes  = map snd checkedArgs
    when (argTypes /= funArgTypes) $ reportError
        ("argument types for function " ++ funName ++ " does not match")
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
        ("incorrect type of left addidtion operand: expected " ++ show TcInt ++ " or " ++
            show TcStr ++ ", got " ++ show lhsType)

checkExpr (ERel lhs relop rhs _) = do
    checkedLhs <- checkExpr lhs
    let lhsType  = snd checkedLhs
    if (relop == EQU || relop == NE) then
        case lhsType of
            TcBool -> do
                typedLhs <- expectType TcBool lhs "left comparison operand"
                typedRhs <- expectType TcBool rhs "right comparison operand"
                return (ERel typedLhs relop typedRhs TcBool, TcBool)
            (TcClass _) -> do
                typedLhs <- expectType lhsType lhs "left comparison operand"
                typedRhs <- expectType lhsType rhs "right comparison operand"
                return (ERel typedLhs relop typedRhs TcBool, TcBool)
            _ -> do
                typedLhs <- expectType TcInt lhs "left comparison operand"
                typedRhs <- expectType TcInt rhs "right comparison operand"
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
beginFunction name = do
    functionType <- getFunctionType name
    modify $ \s -> s {
        currentFunctionType = functionType,
        currentFunctionName = name,
        variables = [],
        innerVariables = []
    }

beginMethod :: String -> String -> Check ()
beginMethod className methodName = do
    methodType <- getMethodType className methodName
    fields <- getClassFields className
    modify $ \s -> s {
        currentFunctionType = methodType,
        currentFunctionName = className ++ "." ++ methodName,
        variables = (("self", (TcClass className)):fields),
        innerVariables = []
    }

-- Constant evaluation (only boolean for now)

exprEval :: Expr -> Maybe Bool
exprEval (ELitTrue _) = Just True
exprEval (ELitFalse _) = Just False
exprEval (EAnd lhs rhs _) = case (exprEval lhs, exprEval rhs) of
    (Just True, Just True) -> Just True
    (Just False, _)        -> Just False
    (_, Just False)        -> Just False
    (_, _)                 -> Nothing
exprEval (EOr lhs rhs _) = case (exprEval lhs, exprEval rhs) of
    (Just False, Just False) -> Just False
    (Just True, _)           -> Just True
    (_, Just True)           -> Just True
    (_, _)                   -> Nothing
exprEval _ = Nothing

-- Auxiliary functions

addFunction :: String -> TcType -> Check ()
addFunction name funType =  do
    functions <- gets functions
    modify $ \s -> s {
        functions = (name, funType):functions
    }

addClass :: String -> ClassFields -> ClassMethods -> Check ()
addClass name fields methods =  do
    classes <- gets classes
    when (lookup name classes /= Nothing)
        (reportError ("multiple definitions of class " ++ name))
    modify $ \s -> s {
        classes = (name, (fields, methods)):classes
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

getMethodType :: String -> String -> Check TcType
getMethodType className methodName = do
    methods <- getClassMethods className
    case lookup methodName methods of
        Just methodType -> return methodType
        Nothing         -> reportError ("Class " ++ className ++ " has no method " ++
            methodName)

getClassFields :: String -> Check [(String, TcType)]
getClassFields className = do
    (ClassFields classFields, _) <- getClassMembers className
    return classFields

getClassMethods :: String -> Check [(String, TcType)]
getClassMethods className = do
    (_, ClassMethods classMethods) <- getClassMembers className
    return classMethods

getClassMembers :: String -> Check (ClassFields, ClassMethods)
getClassMembers className = do
    classes <- gets classes
    case lookup className classes of
        Nothing          -> reportError ("Unknown class " ++ className)
        Just members -> return members

getFieldType :: TcType -> String -> Check TcType
getFieldType (TcArr _) "length" = return TcInt
getFieldType (TcClass className) fieldName = do
    classFields <- getClassFields className
    case lookup fieldName classFields of
        Nothing        -> reportError ("Unknown field "  ++ fieldName)
        Just fieldType -> return fieldType
getFieldType _ fieldName = reportError ("unknown field "  ++ fieldName)

checkEditable :: TcType -> String -> Check ()
checkEditable (TcArr _) "length" = reportError "cannot change array length"
checkEditable (TcClass className) fieldName = return ()

getBuiltinFunctionType :: String -> Maybe TcType
getBuiltinFunctionType "printInt" = Just $ TcFun TcVoid [TcInt]
getBuiltinFunctionType "printString" = Just $ TcFun TcVoid [TcStr]
getBuiltinFunctionType "readInt" = Just $ TcFun TcInt []
getBuiltinFunctionType "readString" = Just $ TcFun TcStr []
getBuiltinFunctionType "error" = Just $ TcFun TcVoid []
getBuiltinFunctionType _ = Nothing

addArg :: Arg -> Check ()
addArg (Ar argType (Ident name)) = do
    variables <- gets variables
    argTcType <- typeToTcType argType
    case lookup name variables of
        Just _  -> reportError $ "Repeated argument name " ++ name
        Nothing -> addVariable name argTcType

addVariable :: String -> TcType -> Check ()
addVariable name varType =  do
    variables <- gets variables
    innerVariables <- gets innerVariables
    when (elem name innerVariables) (reportError $ "multiple declarations of variable " ++ name)
    modify $ \s -> s {
            variables = (name, varType):variables,
            innerVariables = name:innerVariables
        }

setReturned :: Bool -> Check ()
setReturned ret = modify $ \s -> s { returned = ret}

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

typeToTcType :: Type -> Check TcType
typeToTcType Int  = return TcInt
typeToTcType Str  = return TcStr
typeToTcType Bool = return TcBool
typeToTcType Void = return TcVoid
typeToTcType (Arr arrType) = do
    arrTcType <- typeToTcType arrType
    return $ TcArr arrTcType
typeToTcType (Class (Ident className)) =do
    return $ TcClass className

reportError :: String -> StateT CheckState (Either String) b
reportError msg = StateT {
    runStateT = \s -> if (currentFunctionName s) == "" then
            Left ("Type error: " ++ msg)
        else
            Left ("Type error in function " ++ (currentFunctionName s) ++ ": " ++ msg)
}
