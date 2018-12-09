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
import Asm

import Debug.Trace --TODO remove

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
            -- TODO use newFilename
            writeFile "out.s" asm
            system "nasm -f elf64 out.s"
            system "gcc out.o lib/runtime.o -o out"
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
    asm <- generateAsm ir
    --TODO remove printing IR
    trace ((ppShow ir) ++ "\n\n" ++ asm) (return asm)

parse :: String -> Either String Program
parse content = case pProgram $ myLexer content of
        Ok ast -> return ast
        Bad msg -> Left msg
