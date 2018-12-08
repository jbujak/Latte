import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Process

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List

import Ir

data CompileState = CompileState {
    mainBody :: String,
    nextRegister :: Integer,
    vEnv :: [(String, ExpValue)]
}

data ExpValue = Value Integer | Register Integer
data BinOp = Add | Sub | Mul | Div

type Compile a = (StateT CompileState (Either String)) a


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            if ".lat" `isSuffixOf` filename then do
                content <- readFile filename
                compile filename content
            else do
                hPutStrLn stderr "Incorrect extension"
                exitWith $ ExitFailure 255
        _ -> do
            usage
            exitWith $ ExitFailure 255

usage :: IO ()
usage = hPutStrLn stderr $ "usage: ./latc_x86 filename.lat"

compile :: String -> String -> IO ()
compile filename content = do
    case tryCompile content of
        Right asm ->  do
            hPutStrLn stderr asm
            --writeFile newFilename asm
            -- TODO compile asm
            --system $ "llvm-as " ++ newFilename
            return ()
            where newFilename = (changeExtension filename "s")
        Left msg -> do
            hPutStrLn stderr msg
            exitWith $ ExitFailure 255

changeExtension :: String -> String -> String
changeExtension baseFilename newExtension =
    (fst $ splitExtension baseFilename) ++ "." ++ newExtension

tryCompile :: String -> Either String String
tryCompile content = do
    ast <- parse content
    ir  <- generateIr ast
    llvmMainBody <- runCompilation $ compileProgram ast
    return $ show ir

parse :: String -> Either String Program
parse content = case pProgram $ myLexer content of
        Ok ast -> return ast
        Bad msg -> Left msg

runCompilation :: Compile () -> Either String String
runCompilation m = fmap (mainBody . snd) $ runStateT m emptyCompileState

emptyCompileState :: CompileState
emptyCompileState = CompileState {
    mainBody = "",
    nextRegister = 0,
    vEnv = []
}

compileProgram :: Program -> Compile ()
compileProgram (Prog stmts) = return ()

--compileStmt :: Stmt -> Compile ()
--compileStmt (SAss (Ident variable) expression) = do
--    result <- compileExp expression
--    modify $ \s -> s { vEnv = replaceElemKey variable result (vEnv s) }
--compileStmt (SExp expression) = do
--    result <- compileExp expression
--    generatePrintf result
--
--compileExp :: Exp -> Compile ExpValue
--compileExp (ExpAdd lhs rhs) = compileBinOp Add lhs rhs
--compileExp (ExpSub lhs rhs) = compileBinOp Sub lhs rhs
--compileExp (ExpMul lhs rhs) = compileBinOp Mul lhs rhs
--compileExp (ExpDiv lhs rhs) = compileBinOp Div lhs rhs
--compileExp (ExpVar (Ident variable)) = do
--    varReg <- variableRegister variable
--    return $ Register varReg
--compileExp (ExpLit n) = do
--    resultReg <- newRegister
--    generateBinOp Add resultReg (Value n) (Value 0)
--    return $ Register resultReg
--
--compileBinOp :: BinOp -> Exp -> Exp -> Compile ExpValue
--compileBinOp op lhs rhs = do
--    lhsValue <- compileExp lhs
--    rhsValue <- compileExp rhs
--    resultReg <- newRegister
--    generateBinOp op resultReg lhsValue rhsValue
--    return $ Register resultReg
--    
--
--newRegister :: Compile Integer
--newRegister = do
--    reg <- gets nextRegister
--    modify $ \s -> s { nextRegister = reg + 1 }
--    return reg
--
--variableRegister :: String -> Compile Integer
--variableRegister variable = do
--    vEnv <- gets vEnv
--    case lookup variable vEnv of
--        Just (Register reg) -> return reg
--        Nothing             -> reportError $ "compilation error: unkown variable " ++ variable
--
--generateBinOp :: BinOp -> Integer -> ExpValue -> ExpValue -> Compile ()
--generateBinOp op destReg lhs rhs = do
--    generateValue (Register destReg)
--    generateStr $ " = " ++ (getBinOp op) ++ " i32 "
--    generateValue lhs
--    generateStr ", "
--    generateValue rhs
--    generateStr "\n"
--    
--
--generatePrintf :: ExpValue -> Compile ()
--generatePrintf val = do
--    destReg <- newRegister
--    generateValue $ Register destReg
--    generateStr " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds \
--                  \([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 "
--    generateValue val 
--    generateStr ")\n"
--
--
--generateValue :: ExpValue -> Compile ()
--generateValue (Value n) = generateStr $ show n
--generateValue (Register reg) = generateStr $ "%t" ++ (show reg)
--
--generateStr :: String -> Compile ()
--generateStr str = modify $ \s -> s {mainBody = (mainBody s) ++ str}
--
--getBinOp :: BinOp -> String
--getBinOp Add = "add"
--getBinOp Sub = "sub"
--getBinOp Mul = "mul"
--getBinOp Div = "sdiv"
--
--reportError :: String -> Compile a
--reportError msg = StateT { runStateT = \s -> Left msg }
--
--replaceElemKey :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
--replaceElemKey key val list = (key, val):(filter (\x -> fst x /= key) list)

