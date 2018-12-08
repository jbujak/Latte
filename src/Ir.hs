module Ir ( Ir, generateIr) where

import Control.Monad.State
import Control.Applicative

import ParLatte
import PrintLatte
import ErrM
import AbsLatte
import Data.List


type Ir = [IrFunction]

data IrFunction = Function {
    name :: String,
    commands :: [IrCommand]
} deriving (Show)

data IrCommand = Nop | Call String [Integer] deriving (Show)

data GenerateState = State {
    currentReg :: Integer,
    functions :: [IrFunction]
}

type Generate a = (StateT GenerateState (Either String)) a


-- Monad boilerplate

generateIr :: Program -> Either String [IrFunction]
generateIr prog = runGeneration $ generateProgram prog

runGeneration :: Generate () -> Either String [IrFunction]
runGeneration m = fmap (functions . snd) $ runStateT m emptyGenerateState

emptyGenerateState :: GenerateState
emptyGenerateState = State {
    currentReg = 0,
    functions = []
}


-- Ir generator implementation

generateProgram :: Program -> Generate ()
generateProgram prog = do
    modify $ \s -> s {
        functions = [
            Function { name = "main", commands = [Call "printInt" [1]] }
        ]
    }

