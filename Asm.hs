{-# LANGUAGE FlexibleInstances #-}

module Asm
( pp 
, Asm
, Section(..)
, AsmGenT
, runAsmGenT
, rax
, rbx
, rcx
, rdx
, rdi
, rsi
, rsp
, rbp
, push
, pop
, jmp
, syscall
, mov
, add
, sub
, imul
, idiv
, label
, global
, section
, peephole
) where

import Data.String
import Control.Monad.Writer
import Data.List (intercalate)

class Pretty a where
    pp :: a -> String

data Reg = Rax
         | Rbx
         | Rcx
         | Rdx
         | Rdi
         | Rsi
         | Rsp
         | Rbp
         deriving (Eq, Show)

instance Pretty Reg where
    pp Rax = "rax"
    pp Rbx = "rbx"
    pp Rcx = "rcx"
    pp Rdx = "rdx"
    pp Rdi = "rdi"
    pp Rsi = "rsi"
    pp Rsp = "rsp"
    pp Rbp = "rbp"

data Arg = Reg Reg
         | Int Int
         | Label String
         deriving (Eq, Show)

instance IsString Arg where
    fromString = Label

instance Num Arg where
    fromInteger = Int . fromInteger
    _ + _       = undefined
    _ * _       = undefined
    negate      = undefined
    signum      = undefined
    abs         = undefined

instance Pretty Arg where
    pp (Reg r)   = pp r
    pp (Int i)   = show i
    pp (Label l) = l

data Asm = Push Arg
         | Pop Arg
         | Mov Arg Arg
         | Jmp Arg
         | Add Arg Arg
         | Sub Arg Arg
         | IMul Arg Arg
         | IDiv Arg Arg
         | Syscall
         | DefLabel String
         | Global String
         | Section Section
         deriving (Eq, Show)

data Section = Data | Text deriving (Eq, Show)

instance Pretty Asm where
    pp (Push a)      = "\tpush " ++ pp a
    pp (Pop a)       = "\tpop " ++ pp a
    pp (Mov x y)     = "\tmov " ++ pp x ++ ", " ++ pp y
    pp (Jmp x)       = "\tjmp " ++ pp x
    pp (Add x y)     = "\tadd " ++ pp x ++ ", " ++ pp y
    pp (Sub x y)     = "\tsub " ++ pp x ++ ", " ++ pp y
    pp (IMul x y)    = "\timul " ++ pp x ++ ", " ++ pp y
    pp (IDiv x y)    = "\tidiv " ++ pp x ++ ", " ++ pp y
    pp Syscall       = "\tsyscall"
    pp (DefLabel l)  = l ++ ":"
    pp (Global l)    = "global " ++ l
    pp (Section t)   = "section " ++ if t == Text then ".text" else ".data"

instance Pretty [Asm] where
    pp = intercalate "\n" . map pp

type AsmGenT = WriterT [Asm] 

runAsmGenT :: (Monad m) => AsmGenT m a -> m [Asm]
runAsmGenT = execWriterT

rax, rbx, rcx, rdx :: Arg
rax = Reg Rax
rbx = Reg Rbx
rcx = Reg Rcx
rdx = Reg Rdx
rdi = Reg Rdi
rsi = Reg Rsi
rsp = Reg Rsp
rbp = Reg Rbp

syscall :: (Monad m) => AsmGenT m ()
syscall = tell [Syscall]
push, pop, jmp :: (Monad m) => Arg -> AsmGenT m ()
push x = tell [Push x]
pop x  = tell [Pop x]
jmp x  = tell [Jmp x]
mov, add, sub, imul, idiv :: (Monad m) => Arg -> Arg -> AsmGenT m ()
mov x y = tell [Mov x y]
add x y = tell [Add x y]
sub x y = tell [Sub x y]
imul x y = tell [IMul x y]
idiv x y = tell [IDiv x y]
label, global :: (Monad m) => String -> AsmGenT m ()
label x  = tell [DefLabel x]
global x = tell [Global x]
section :: (Monad m) => Section -> AsmGenT m ()
section s = tell [Section s]

peephole :: [Asm] -> [Asm]
peephole []                    = []
peephole ((Push x):(Pop r):xs) = Mov r x : peephole xs
peephole (x:xs)                = x : peephole xs

