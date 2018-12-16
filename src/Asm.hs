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

data Value = Reg Register | Loc Local | Int Integer

data Register = RAX | RBX | RCX | RDX | RBP | RSP | RSI | RDI | 
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

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
    programProlog
    forM_ ir generateAsmFromFunction

generateAsmFromFunction :: IrFunction -> Generate ()
generateAsmFromFunction function = do
    functionProlog function
    forM_ (commands function) generateAsmFromCommand
    functionEpilog

generateAsmFromCommand :: IrCommand -> Generate ()
generateAsmFromCommand Nop =
    nop
generateAsmFromCommand (LoadConst local (ConstInt value)) =
    mov (Loc local) (Int value)
generateAsmFromCommand (Call local name args) = do
    generateLoadArgs args
    call name
generateAsmFromCommand (BinOp result lhs rhs op) = do
    mov (Reg R8) (Loc lhs)
    mov (Reg R9) (Loc rhs)
    binOp R8 R9 op
    mov (Loc result) (Reg R8)
generateAsmFromCommand (Goto label) =
    jmp label
generateAsmFromCommand (GotoIf local label) = do
    mov (Reg R8) (Loc local)
    cmp R8 "0"
    jne label
generateAsmFromCommand (PrintLabel label) =
    printLabel label

generateAsmFromCommand cmd = return ()

generateLoadArgs :: [Local] -> Generate()
generateLoadArgs args = generateLoadArgsInner args 0 where
    generateLoadArgsInner [] _ = return ()
    generateLoadArgsInner (local:locals) argNo = loadArgument local argNo



-- Asm macros

programProlog :: Generate ()
programProlog = do
    extern "printInt"
    extern "printString"
    global "main"

functionProlog :: IrFunction -> Generate ()
functionProlog function = do
    printStr $ (name function) ++ ":\n"
    push RBP
    mov (Reg RBP) (Reg RSP)
    sub RSP (Int stackSize)
    where stackSize = alignStackSize (locals function * localSize)

functionEpilog :: Generate ()
functionEpilog = do
    asmLabel exitLabel
    mov (Reg RSP) (Reg RBP)
    pop RBP
    ret

loadArgument :: Local -> Integer -> Generate ()
loadArgument local argNo = case registerForArgument argNo of
    Just register -> mov (Reg register) (Loc local)

binOp :: Register -> Register -> BinOpType -> Generate ()
binOp lhs rhs Add = add  lhs (Reg rhs)
binOp lhs rhs Sub = sub  lhs (Reg rhs)
binOp lhs rhs Mul = imul lhs (Reg rhs)
binOp lhs rhs Div = do
    xor RDX RDX
    mov (Reg RAX) (Reg lhs)
    idiv rhs
    mov (Reg lhs) (Reg RAX)
binOp lhs rhs Mod = do
    xor RDX RDX
    mov (Reg RAX) (Reg lhs)
    idiv rhs
    mov (Reg lhs) (Reg RDX)

registerForArgument :: Integer -> Maybe Register
registerForArgument 0 = Just RDI
registerForArgument 1 = Just RSI
registerForArgument 2 = Just RDX
registerForArgument 3 = Just RCX
registerForArgument 4 = Just R8
registerForArgument 5 = Just R9
registerForArgument _ = Nothing --TODO

exitLabel :: String
exitLabel = ".exit"

labelName :: Label -> String
labelName label = "label_" ++ show label



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



-- Asm commands

nop :: Generate ()
nop = asmLine ["nop"]

mov :: Value -> Value -> Generate ()
mov (Reg dstReg) (Reg srcReg) = asmLine ["mov", show dstReg, ",", show srcReg]
mov (Reg dstReg) (Loc srcLocal) = asmLine ["mov", show dstReg, ",", getLocal srcLocal]
mov (Loc dstLocal) (Reg srcReg) = asmLine ["mov", getLocal dstLocal, ",", show srcReg]
mov (Loc dstLocal) (Int srcInt) =
    asmLine ["mov", "QWORD", getLocal dstLocal, ",", show srcInt]

cmp :: Register -> String -> Generate ()
cmp lhs rhs = asmLine ["cmp", show lhs, ", ", rhs]

jmp :: Label -> Generate ()
jmp label = asmLine ["jmp", labelName label]

jne :: Label -> Generate ()
jne label = asmLine ["jne", labelName label]

xor :: Register -> Register -> Generate ()
xor lhs rhs = asmLine ["xor", show lhs, ",", show rhs]

idiv :: Register -> Generate ()
idiv reg = asmLine ["idiv", show reg]

push :: Register -> Generate ()
push reg = asmLine ["push", show reg]

pop :: Register -> Generate ()
pop reg = asmLine ["pop", show reg]

ret :: Generate ()
ret = asmLine ["ret"]

add :: Register -> Value -> Generate ()
add lhs (Reg rhs) = asmLine ["add", show lhs, ", ", show rhs]

sub :: Register -> Value -> Generate ()
sub lhs (Reg rhs) = asmLine ["sub", show lhs, ", ", show rhs]
sub lhs (Int int) = asmLine ["sub", show lhs, ", ", show int]

imul :: Register -> Value -> Generate ()
imul lhs (Reg rhs) = asmLine ["imul", show lhs, ", ", show rhs]

call :: String -> Generate ()
call funName = asmLine ["call", funName]

extern :: String -> Generate ()
extern name = printStr $ "extern " ++ name ++ "\n"

global :: String -> Generate ()
global name = printStr $ "global " ++ name ++ "\n"

printLabel :: Label -> Generate ()
printLabel label = asmLabel $ labelName label

asmLabel :: String -> Generate ()
asmLabel label = printStr $ "  " ++ label ++ ":\n"

asmLine :: [String] -> Generate ()
asmLine parts = printStr $ "    " ++ (intercalate " " parts) ++ "\n"

instance Show Register where
    show RAX = "rax"
    show RBX = "rbx"
    show RCX = "rcx"
    show RDX = "rdx"
    show RBP = "rbp"
    show RSP = "rsp"
    show RSI = "rsi"
    show RDI = "rdi"
    show R8  = "r8"
    show R9  = "r9"
    show R10 = "r10"
    show R11 = "r11"
    show R12 = "r12"
    show R13 = "r13"
    show R14 = "r14"
    show R15 = "r15"
