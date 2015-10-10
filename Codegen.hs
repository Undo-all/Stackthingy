{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Asm
import Ast
import Error
import Data.String
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S

data CodegenState = CodegenState
                  { anonBlocks  :: Int
                  , calls       :: Int
                  , namedBlocks :: S.Set String
                  } deriving (Eq, Show)

type Codegen = AsmGenT (StateT CodegenState (Except Error))

runCodegen :: Codegen a -> Either Error [Asm]
runCodegen cg = runExcept $ evalStateT (runAsmGenT cg) defaultState
  where defaultState = CodegenState 0 0 S.empty

generate :: [Expr] -> Codegen ()
generate xs = do
    modify $ \cg -> cg { namedBlocks = findNames xs }
    section Text
    global "_start"
    label "_start"
    mapM_ gen xs
    mov rax 1
    mov rbx 0
    int 0x80
  where findNames                     = foldl insertName S.empty
        insertName s (NamedBlock n _) = S.insert n s
        insertName s _                = s

binop x = do
    pop rax
    pop rbx
    x rax rbx
    push rax

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
gen (Lit (Ident l)) = do
    names <- gets namedBlocks
    if S.member l names
      then push (fromString ('_':l))
      else throwError (NotFound l)

gen Add = binop add
gen Sub = binop sub
gen Mul = binop mul
gen Div = binop idiv

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

