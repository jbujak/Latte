module Asm ( generateAsm ) where

import Control.Monad.State
import Control.Applicative

import Data.List

import Ir


-- Types definition

data GenerateState = State {
    output :: String,
    currentFunction :: String
}

type Generate a = (StateT GenerateState (Either String)) a

data Value = Reg Register | Loc Local | Int Integer | Ptr String

data Register = RAX | RBX | RCX | RDX | RBP | RSP | RSI | RDI | 
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

-- Monad boilerplate

generateAsm :: Ir -> Either String String
generateAsm ir = runGeneration $ generateAsmFromIr ir

runGeneration :: Generate () -> Either String String
runGeneration m = fmap (output . snd) $ runStateT m emptyGenerateState

emptyGenerateState :: GenerateState
emptyGenerateState = State {
    output = "",
    currentFunction = ""
}


-- Asm generator implementation

generateAsmFromIr :: Ir -> Generate ()
generateAsmFromIr ir = do
    programProlog ir
    forM_ (irFunctions ir) generateAsmFromFunction

generateAsmFromFunction :: IrFunction -> Generate ()
generateAsmFromFunction function = do
    functionProlog function
    modify $ \s -> s { currentFunction = name function }
    forM_ (commands function) generateAsmFromCommand
    functionEpilog

generateAsmFromCommand :: IrCommand -> Generate ()
generateAsmFromCommand Nop =
    nop
generateAsmFromCommand (LoadConst local (ConstInt value)) =
    mov (Loc local) (Int value)
generateAsmFromCommand (LoadConst local (ConstString stringNo)) = do
    let (Just firstArg) = registerForArgument 0
    mov (Reg firstArg) (Ptr $ stringLabel stringNo)
    call strcpy_to_new
    mov (Loc local) (Reg RAX)
generateAsmFromCommand (Call local name args) = do
    generateLoadArgsToRegs args
    call name
    mov (Loc local) (Reg RAX)
generateAsmFromCommand (LoadArgs []) = return ()
generateAsmFromCommand (LoadArgs args) =
    generateLoadArgsToLocals args
generateAsmFromCommand (Return (Just local)) = do
    mov (Reg RAX) (Loc local)
    exitLabelStr <- exitLabel
    jmpSpecial exitLabelStr
generateAsmFromCommand (Return Nothing) = do
    mov (Reg RAX) (Int 0)
    exitLabelStr <- exitLabel
    jmpSpecial exitLabelStr
generateAsmFromCommand (BinOp result lhs rhs op) = do
    mov (Reg R8) (Loc lhs)
    mov (Reg R9) (Loc rhs)
    binOp R8 R9 op
    mov (Loc result) (Reg R8)
generateAsmFromCommand (Goto label) =
    jmp label
generateAsmFromCommand (GotoIf local label) = do
    mov (Reg R8) (Loc local)
    cmp R8 (Int 0)
    jne label
generateAsmFromCommand (PrintLabel label) =
    printLabel label
generateAsmFromCommand (Assign dstLocal srcLocal) = do
    mov (Reg R8) (Loc srcLocal)
    mov (Loc dstLocal) (Reg R8)

generateLoadArgsToRegs :: [Local] -> Generate()
generateLoadArgsToRegs args = generateLoadArgsInner args 0 where
    generateLoadArgsInner [] _ = return ()
    generateLoadArgsInner (local:locals) argNo = do
        loaded <- loadArgumentToReg local argNo
        if (loaded) then
            generateLoadArgsInner locals (argNo + 1)
        else do
            when (length (local:locals) `mod` 2 /= 0) (push RAX)
            generateLoadArgsToStack (local:locals)

generateLoadArgsToStack :: [Local] -> Generate()
generateLoadArgsToStack [] = return ()
generateLoadArgsToStack (arg:args) = do
    generateLoadArgsToStack args
    mov (Reg R10) (Loc arg)
    push R10


generateLoadArgsToLocals :: [Local] -> Generate ()
generateLoadArgsToLocals args = generateLoadArgsInner args 0 where
    generateLoadArgsInner [] _ = return ()
    generateLoadArgsInner (local:locals) argNo = do
        loaded <- loadArgumentFromReg local argNo
        if (loaded) then
            generateLoadArgsInner locals (argNo + 1)
        else
            generateLoadArgsFromStack (local:locals)

generateLoadArgsFromStack :: [Local] -> Generate ()
generateLoadArgsFromStack args = generateLoadArgsInner args 0 where
    generateLoadArgsInner [] _ = return ()
    generateLoadArgsInner (local:locals) argNo = do
        mov (Reg R10) (Ptr (getStackArg argNo))
        mov (Loc local) (Reg R10)
        generateLoadArgsInner locals (argNo + 1)


-- Asm macros

programProlog :: Ir -> Generate ()
programProlog ir = do
    externFunctions
    global "main"
    section ".rodata"
    programData ir
    section ".text"

functionProlog :: IrFunction -> Generate ()
functionProlog function = do
    printStr $ (name function) ++ ":\n"
    push RBP
    mov (Reg RBP) (Reg RSP)
    sub RSP (Int stackSize)
    where stackSize = alignStackSize (locals function * localSize)

functionEpilog :: Generate ()
functionEpilog = do
    exitLabelStr <- exitLabel
    asmLabel exitLabelStr
    mov (Reg RSP) (Reg RBP)
    pop RBP
    ret

externFunctions :: Generate()
externFunctions = do
    extern "printInt"
    extern "printString"
    extern "error"
    extern "readInt"
    extern "readString"
    extern strcpy_to_new
    extern strcat_to_new

programData :: Ir -> Generate ()
programData ir = programDataInner strings ((toInteger $ length strings)-1) where
    strings = stringLiterals ir
    programDataInner [] _ = return ()
    programDataInner (str:strs) n = do
        stringConst str n
        programDataInner strs (n-1)

stringConst :: String -> Integer -> Generate ()
stringConst str strNo =
    printStr $ (stringLabel strNo) ++ " db \"" ++ str ++ "\", 0\n"

stringLabel :: Integer -> String
stringLabel strNo = "str_" ++ show strNo

section :: String -> Generate ()
section name = printStr $ "section " ++ name ++ "\n"

loadArgumentToReg :: Local -> Integer -> Generate Bool
loadArgumentToReg local argNo = case registerForArgument argNo of
    Just register -> mov (Reg register) (Loc local) >> return True
    Nothing       -> return False

loadArgumentFromReg :: Local -> Integer -> Generate Bool
loadArgumentFromReg local argNo = case registerForArgument argNo of
    Just register -> mov (Loc local) (Reg register) >> return True
    Nothing       -> return False

-- binOp semantics: lhs = lhs `op` rhs
binOp :: Register -> Register -> BinOpType -> Generate ()
binOp lhs rhs Add = add  lhs (Reg rhs)
binOp lhs rhs Sub = sub  lhs (Reg rhs)
binOp lhs rhs Mul = imul lhs (Reg rhs)
binOp lhs rhs And = Asm.and  lhs (Reg rhs)
binOp lhs rhs Or  = Asm.or   lhs (Reg rhs)
binOp lhs rhs Xor = Asm.xor  lhs (Reg rhs)
binOp lhs rhs Div = divResultFrom lhs rhs RAX
binOp lhs rhs Mod = divResultFrom lhs rhs RDX
binOp lhs rhs Eq  = compareEqual lhs lhs rhs False
binOp lhs rhs Neq = compareEqual lhs lhs rhs True
binOp lhs rhs Lt  = compareLess lhs lhs rhs False
binOp lhs rhs Gte = compareLess lhs lhs rhs True
binOp lhs rhs Gt  = compareLess lhs rhs lhs False
binOp lhs rhs Lte = compareLess lhs rhs lhs True
binOp lhs rhs Concat = do
    let (Just firstArg)  = registerForArgument 0
    let (Just secondArg) = registerForArgument 1
    mov (Reg firstArg)  (Reg lhs)
    mov (Reg secondArg) (Reg rhs)
    call strcat_to_new
    mov (Reg lhs) (Reg RAX)


divResultFrom :: Register -> Register -> Register -> Generate ()
divResultFrom lhs rhs result = do
    xor RDX (Reg RDX)
    mov (Reg RAX) (Reg lhs)
    idiv rhs
    mov (Reg lhs) (Reg result)

compareEqual :: Register -> Register -> Register -> Bool -> Generate ()
compareEqual outReg lhs rhs negate = do
    xor RAX (Reg RAX)
    cmp lhs (Reg rhs)
    lahf
    shr RAX zfBit
    Asm.and RAX (Int 0x1)
    when negate $ xor RAX (Int 0x1)
    mov (Reg outReg) (Reg RAX)

compareLess :: Register -> Register -> Register -> Bool -> Generate ()
compareLess outReg lhs rhs negate = do
    xor RAX (Reg RAX)
    cmp lhs (Reg rhs)
    lahf
    mov (Reg RCX) (Reg RAX)
    shr RAX sfBit
    shr RCX ofBit
    Asm.and RAX (Int 0x1)
    Asm.and RCX (Int 0x1)
    xor RAX (Reg RCX)
    when negate $ xor RAX (Int 0x1)
    mov (Reg outReg) (Reg RAX)

registerForArgument :: Integer -> Maybe Register
registerForArgument 0 = Just RDI
registerForArgument 1 = Just RSI
registerForArgument 2 = Just RDX
registerForArgument 3 = Just RCX
registerForArgument 4 = Just R8
registerForArgument 5 = Just R9
registerForArgument _ = Nothing --TODO

exitLabel :: Generate String
exitLabel = do
    funName <- gets currentFunction
    return $ funName ++ "_exit"

labelName :: Label -> Generate String
labelName label = do
    funName <- gets currentFunction
    return $ funName ++ "_label_" ++ show label

-- Auxiliary internal functions

strcpy_to_new :: String
strcpy_to_new = "_latte_strcpy_to_new"

strcat_to_new :: String
strcat_to_new = "_latte_strcat_to_new"


-- Auxiliary functions

alignStackSize :: Integer -> Integer
alignStackSize stackSize = 16 * ((stackSize + 15) `div` 16)

getLocal :: Local -> String
getLocal local = "[rbp-" ++ show offset ++ "]"
    where offset = local * localSize

getStackArg :: Integer -> String
getStackArg argNo = "[rbp+" ++ show (offset + 16) ++ "]"
    where offset = argNo * localSize

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
mov (Reg dstReg) (Int srcInt) = asmLine ["mov", show dstReg, ",", show srcInt]
mov (Loc dstLocal) (Reg srcReg) = asmLine ["mov", getLocal dstLocal, ",", show srcReg]
mov (Loc dstLocal) (Int srcInt) =
    asmLine ["mov", "QWORD", getLocal dstLocal, ",", show srcInt]
mov (Reg dstReg) (Ptr ptrName) =
    asmLine ["mov", "QWORD", show dstReg, ",", ptrName]

cmp :: Register -> Value -> Generate ()
cmp lhs (Int rhs) = asmLine ["cmp", show lhs, ", ", show rhs]
cmp lhs (Reg rhs) = asmLine ["cmp", show lhs, ", ", show rhs]

jmp :: Label -> Generate ()
jmp label = do
    labelStr <- labelName label
    jmpSpecial labelStr

jmpSpecial :: String -> Generate ()
jmpSpecial labelStr = asmLine ["jmp", labelStr]

jne :: Label -> Generate ()
jne label = do
    labelStr <- labelName label
    asmLine ["jne", labelStr]

idiv :: Register -> Generate ()
idiv reg = asmLine ["idiv", show reg]

push :: Register -> Generate ()
push reg = asmLine ["push", show reg]

pop :: Register -> Generate ()
pop reg = asmLine ["pop", show reg]

ret :: Generate ()
ret = asmLine ["ret"]

add :: Register -> Value -> Generate ()
add lhs rhs = asmBinOp "add" lhs rhs

sub :: Register -> Value -> Generate ()
sub lhs rhs = asmBinOp "sub" lhs rhs

imul :: Register -> Value -> Generate ()
imul lhs rhs = asmBinOp "imul" lhs rhs

inc :: Register -> Generate ()
inc reg = asmLine ["inc", show reg]

and :: Register -> Value -> Generate ()
and lhs rhs = asmBinOp "and" lhs rhs

or :: Register -> Value -> Generate ()
or lhs rhs = asmBinOp "or" lhs rhs

xor :: Register -> Value -> Generate ()
xor lhs rhs = asmBinOp "xor" lhs rhs

call :: String -> Generate ()
call funName = asmLine ["call", funName]

lahf :: Generate ()
lahf = asmLine ["lahf"]

zfBit :: Integer
zfBit = 14

sfBit :: Integer
sfBit = 15

ofBit :: Integer
ofBit = 19

neg :: Register -> Generate ()
neg reg = asmLine ["neg", show reg]

shr :: Register -> Integer -> Generate ()
shr lhs rhs = asmLine ["shr", show lhs, ",", show rhs]

extern :: String -> Generate ()
extern name = printStr $ "extern " ++ name ++ "\n"

global :: String -> Generate ()
global name = printStr $ "global " ++ name ++ "\n"

printLabel :: Label -> Generate ()
printLabel label = do
    labelStr <- labelName label
    asmLabel labelStr

asmBinOp :: String -> Register -> Value -> Generate ()
asmBinOp op lhs (Reg rhs) = asmLine [op, show lhs, ",", show rhs]
asmBinOp op lhs (Int rhs) = asmLine [op, show lhs, ",", show rhs]

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
