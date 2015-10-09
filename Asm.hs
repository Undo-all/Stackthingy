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
, push
, pop
, jmp
, int
, mov
, add
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
         deriving (Eq, Show)

instance Pretty Reg where
    pp Rax = "rax"
    pp Rbx = "rbx"
    pp Rcx = "rcx"
    pp Rdx = "rdx"

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
         | Interrupt Arg
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
    pp (Interrupt x) = "\tint " ++ pp x
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

push, pop, jmp, int :: (Monad m) => Arg -> AsmGenT m ()
push x = tell [Push x]
pop x  = tell [Pop x]
jmp x  = tell [Jmp x]
int x  = tell [Interrupt x]
mov, add :: (Monad m) => Arg -> Arg -> AsmGenT m ()
mov x y = tell [Mov x y]
add x y = tell [Add x y]
label, global :: (Monad m) => String -> AsmGenT m ()
label x  = tell [DefLabel x]
global x = tell [Global x]
section :: (Monad m) => Section -> AsmGenT m ()
section s = tell [Section s]

peephole :: [Asm] -> [Asm]
peephole []                    = []
peephole ((Push x):(Pop r):xs) = Mov r x : peephole xs
peephole (x:xs)                = x : peephole xs

