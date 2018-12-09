module Asm ( generateAsm ) where

import Control.Monad.State
import Control.Applicative

import Data.List

import Ir


-- Types definition

data GenerateState = State {
    output :: String
}

type Generate a = (StateT GenerateState (Either String)) a


-- Monad boilerplate

generateAsm :: Ir -> Either String String
generateAsm ir = runGeneration $ generateAsmFromIr ir

runGeneration :: Generate () -> Either String String
runGeneration m = fmap (output . snd) $ runStateT m emptyGenerateState

emptyGenerateState :: GenerateState
emptyGenerateState = State {
    output = ""
}


-- Asm generator implementation

generateAsmFromIr :: Ir -> Generate ()
generateAsmFromIr ir = do
    printStr programProlog
    forM_ ir generateAsmFromFunction

generateAsmFromFunction :: IrFunction -> Generate ()
generateAsmFromFunction function = do
    printStr $ functionProlog function
    forM_ (commands function) generateAsmFromCommand
    printStr functionEpilog

generateAsmFromCommand :: IrCommand -> Generate ()
generateAsmFromCommand (LoadConst local (ConstInt value)) =
    printStr (loadIntToStack local value)
generateAsmFromCommand (Call local name args) = do
    generateLoadArgs args
    printStr (callFun name)

generateAsmFromCommand cmd = return ()

generateLoadArgs :: [Local] -> Generate()
generateLoadArgs args = generateLoadArgsInner args 0 where
    generateLoadArgsInner [] _ = return ()
    generateLoadArgsInner (local:locals) argNo = do
        printStr (loadArgument local argNo)

-- Asm macros

programProlog :: String
programProlog = "extern printInt\n\
                \extern printString\n\
                \\n\
                \global main\n\
                \\n"

functionProlog :: IrFunction -> String
functionProlog function = (name function) ++ ":\n\
                          \    push rbp\n\
                          \    mov rbp, rsp\n\
                          \    sub rsp, " ++ (show stackSize) ++ "\n\
                          \\n"
                          where stackSize = alignStackSize (locals function * localSize)

functionEpilog :: String
functionEpilog = "\n\
                 \.exit:\n\
                 \    mov rsp, rbp\n\
                 \    pop rbp\n\
                 \    ret\n"

loadIntToStack :: Local -> Integer -> String
loadIntToStack local value =
    "    mov QWORD " ++ getLocal local ++ ", " ++ show value ++ "\n"

loadArgument :: Local -> Integer -> String
loadArgument local argNo = case registerForArgument argNo of
    Just register -> "    mov " ++ register ++ ", " ++ getLocal local ++ "\n"
    Nothing       -> "TODO"

callFun :: String -> String
callFun funName = "    call " ++ funName ++ "\n"

loadResult :: Local -> String
loadResult local = "    mov " ++ getLocal local ++ ", rax"

registerForArgument :: Integer -> Maybe String
registerForArgument 0 = Just "rdi"
registerForArgument 1 = Just "rsi"
registerForArgument 2 = Just "rdx"
registerForArgument 3 = Just "rcx"
registerForArgument 4 = Just "r8"
registerForArgument 5 = Just "r9"
--TODO more arguments

alignStackSize :: Integer -> Integer
alignStackSize stackSize = 16 * ((stackSize + 15) `div` 16)

getLocal :: Local -> String
getLocal local = "[rbp-" ++ show offset ++ "]"
    where offset = local * localSize

localSize :: Integer
localSize = 8

-- Auxiliary functions
printStr :: String -> Generate ()
printStr str = modify $ \s -> s { output = output s ++ str }
