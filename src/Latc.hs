import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Process

import Text.Show.Pretty

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
                failExit
        _ -> do
            usage
            failExit

usage :: IO ()
usage = hPutStrLn stderr $ "usage: ./latc_x86_64 filename.lat"

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
            failExit

failExit :: IO ()
failExit = exitWith $ ExitFailure 255

changeExtension :: String -> String -> String
changeExtension baseFilename newExtension =
    (fst $ splitExtension baseFilename) ++ "." ++ newExtension

tryCompile :: String -> Either String String
tryCompile content = do
    ast <- parse content
    ir  <- generateIr ast
    llvmMainBody <- runCompilation $ compileProgram ast
    return $ ppShow ir

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
