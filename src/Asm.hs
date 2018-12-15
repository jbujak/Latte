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
generateAsmFromCommand Nop =
    printStr $ nop
generateAsmFromCommand (LoadConst local (ConstInt value)) =
    printStr $ loadIntToStack local value
generateAsmFromCommand (Call local name args) = do
    generateLoadArgs args
    printStr $ callFun name
generateAsmFromCommand (BinOp result lhs rhs op) = do
    printStr $ moveToReg "r8" lhs
    printStr $ moveToReg "r9" rhs
    printStr $ binOp "r8" "r9" op
    printStr $ moveToLocal result "r8"
generateAsmFromCommand (Goto label) =
    printStr $ jmp label
generateAsmFromCommand (GotoIf local label) = do
    printStr $ moveToReg "r8" local
    printStr $ cmp "r8" "0"
    printStr $ jne label
generateAsmFromCommand (PrintLabel label) =
    printStr $ getLabel label

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
functionEpilog = "  .exit:\n\
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
loadResult local = moveToLocal local "rax"

moveToReg :: String -> Local -> String
moveToReg reg local = "    mov " ++ reg ++ ", " ++ getLocal local ++ "\n"

moveToLocal :: Local -> String -> String
moveToLocal local reg = "    mov " ++ getLocal local ++ ", " ++ reg ++ "\n"

zeroReg :: String -> String
zeroReg reg = "    xor " ++ reg ++ ", " ++ reg ++ "\n"

binOp :: String -> String -> BinOpType -> String
binOp lhs rhs Add = "    add " ++ lhs ++ ", " ++ rhs ++ "\n"
binOp lhs rhs Sub = "    sub " ++ lhs ++ ", " ++ rhs ++ "\n"
binOp lhs rhs Mul = "    imul " ++ lhs ++ ", " ++ rhs ++ "\n"
binOp lhs rhs Div =
    zeroReg "rdx" ++
    "    mov rax, " ++ lhs ++ "\n" ++
    "    idiv " ++ rhs ++ "\n" ++
    "    mov " ++ lhs ++ ", rax\n"
binOp lhs rhs Mod =
    zeroReg "rdx" ++
    "    mov rax, " ++ lhs ++ "\n" ++
    "    idiv " ++ rhs ++ "\n" ++
    "    mov " ++ lhs ++ ", rdx\n"

registerForArgument :: Integer -> Maybe String
registerForArgument 0 = Just "rdi"
registerForArgument 1 = Just "rsi"
registerForArgument 2 = Just "rdx"
registerForArgument 3 = Just "rcx"
registerForArgument 4 = Just "r8"
registerForArgument 5 = Just "r9" --TODO more arguments

labelName :: Label -> String
labelName label = "label_" ++ show label


-- Asm commands

nop :: String
nop = "    nop\n"

cmp :: String -> String -> String
cmp lhs rhs = "    cmp " ++ lhs ++ ", " ++ rhs ++ "\n"

jmp :: Label -> String
jmp label = "    jmp " ++ labelName label ++ "\n"

jne :: Label -> String
jne label = "    jne " ++ labelName label ++ "\n"

getLabel :: Label -> String
getLabel label = "  " ++ labelName label ++ ":\n"

-- Auxiliary functions

alignStackSize :: Integer -> Integer
alignStackSize stackSize = 16 * ((stackSize + 15) `div` 16)

getLocal :: Local -> String
getLocal local = "[rbp-" ++ show offset ++ "]"
    where offset = local * localSize

localSize :: Integer
localSize = 8

printStr :: String -> Generate ()
printStr str = modify $ \s -> s { output = output s ++ str }
