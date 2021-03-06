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

import Text.Show.Pretty --TODO remove
import Debug.Trace --TODO remove

import TypeChecker
import Ir
import Asm

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
                hPutStrLn stderr "ERROR\nIncorrect extension"
                failExit
        _ -> do
            usage
            failExit

usage :: IO ()
usage = hPutStrLn stderr $ "ERROR\nusage: ./latc_x86_64 filename.lat"

compile :: String -> String -> IO ()
compile filename content = do
    case tryCompile content of
        Right asm ->  do
            writeFile asmFilename asm
            system ("nasm -f elf64 -F dwarf -g " ++ asmFilename)
            system ("gcc -no-pie -g " ++ objFilename ++ " lib/runtime.o -o " ++ binFilename)
            system ("rm " ++ objFilename)
            hPutStrLn stderr "OK\n"
            where asmFilename = (changeExtension filename "s")
                  objFilename = (changeExtension filename "o")
                  binFilename = (removeExtension filename)
        Left msg -> do
            hPutStrLn stderr ("ERROR\n" ++ msg)
            failExit

failExit :: IO ()
failExit = exitWith $ ExitFailure 255

changeExtension :: String -> String -> String
changeExtension baseFilename newExtension =
    (removeExtension baseFilename) ++ "." ++ newExtension

removeExtension :: String -> String
removeExtension baseFilename = fst $ splitExtension baseFilename

tryCompile :: String -> Either String String
tryCompile content = do
    ast <- parse content
    (ast, classes) <- checkTypes ast
    ir  <- generateIr ast classes
    asm <- generateAsm ir
    trace ((ppShow ir) ++ "\n\n" ++ (ppShow classes) ++ "\n\n" ++ asm) (return asm)
    return asm

parse :: String -> Either String Program
parse content = case pProgram $ myLexer content of
        Ok ast -> return ast
        Bad msg -> Left msg
