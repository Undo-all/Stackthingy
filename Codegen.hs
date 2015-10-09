{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Asm
import Ast
import Data.String
import Control.Monad.State
import qualified Data.Map as M

data CodegenState = CodegenState
                  { anonBlocks :: Int
                  , calls      :: Int
                  } deriving (Eq, Show)

type Codegen = AsmGenT (State CodegenState)

runCodegen :: Codegen a -> [Asm]
runCodegen cg = evalState (runAsmGenT cg) defaultState
  where defaultState = CodegenState 0 0

binop x = do
    pop rax
    pop rbx
    x rax rbx
    push rax

generate :: [Expr] -> Codegen ()
generate xs = do
    section Text
    global "_start"
    label "_start"
    mapM_ gen xs
    mov rax 1
    mov rbx 0
    int 0x80

genBlock :: String -> [Expr] -> Codegen ()
genBlock blk body = do
    let end = blk ++ "_end"
    jmp (fromString end)
    label blk
    mapM_ gen body
    jmp rdx
    label end

gen :: Expr -> Codegen ()
gen (Lit (Int i))   = push (fromIntegral i)
gen (Lit (Ident l)) = push (fromString ('_':l))

gen Add = binop add
gen (NamedBlock name xs) = genBlock ('_':name) xs
gen (AnonBlock xs) = do
    n <- gets anonBlocks
    modify $ \cg -> cg { anonBlocks = n + 1 }
    let name = "anonblk" ++ show n
    genBlock name xs
    push (fromString name)

gen Call = do
    n <- gets calls
    modify $ \cg -> cg { calls = n + 1 }
    let after = "after_call_" ++ show n
    pop rax
    mov rdx (fromString after)
    jmp rax
    label after

