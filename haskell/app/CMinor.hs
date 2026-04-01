module CMinor where

import Program
import GHC.Natural (Natural)

type Label = Ident

data Constant
  = OIntConst Int |
    OFloatConst Float |
    OSingleConst Float | -- fixeme!! float32
    OLongConst Int |
    OAddRSymbol Ident PtrOfs |
    OAddRStack PtrOfs

data UnaryOperation
  = OCast8Unsigned |
    OCast8Signed |
    OCast16Unsigned |
    OCast16Signed |
    ONegInt |
    ONotInt |
    ONegF |
    OAbsF |
    ONegFs |
    OAbsFs |
    OSingleOfFloat |
    OFloatOfSingle |
    OIntOfFloat |
    OIntUOfFloat |
    OFloatOfInt |
    OFloatOfIntU |
    OIntOfSingle |
    OIntUOfSingle |
    OSingleOfInt |
    OSingleOfIntU |
    ONegl |
    ONotl |
    OIntOfLong |
    OLongOfInt |
    OLongOfIntU |
    OLongOfFLoat |
    OLongUOfFLoat |
    OFLoatOfLong |
    OFloatOfLongU |
    OLongOfSingle |
    OLongUOfSingle |
    OSingleOfLong |
    OSingleOfLongU

data BinaryOperation
  = OAdd |
    OSub |
    OMul |
    ODiv |
    ODivU |
    OMod |
    OModU |
    OAnd |
    OOr |
    OXor |
    OShL |
    OShR |
    OShRU |
    OAddf |
    OSubf |
    OMulf |
    ODivf |
    OAddfs |
    OSubfs |
    OMulfs |
    ODivfs |
    OAddl |
    OSubl |
    OMull |
    ODivl |
    ODivlu |
    OModl |
    OModlu |
    OAndl |
    OOrl |
    OXorl |
    Oshll |
    Oshrl |
    Oshrlu |
    Ocmp Comparison |
    Ocmpu Comparison |
    Ocmpf Comparison |
    Ocmpfs Comparison |
    Ocmpl Comparison |
    Ocmplu Comparison



data Expr
  = EVar Ident |
    EConst Constant |
    EUnop UnaryOperation Expr |
    EBinop BinaryOperation Expr Expr |
    ELoad MemoryChunk Expr Expr

data Stmt
    = Sskip |
      Sassign Ident Expr |
      Sstore MemoryChunk Expr Expr |
      Scall (Maybe Ident) Signature Expr [Expr] |
      Stailcall Signature Expr [Expr] |
      Sbuiltin (Maybe Ident) ExternalFunction [Expr] |
      Sseq Stmt Stmt |
      Sifthenelse Expr Stmt Stmt |
      Sloop Stmt |
      Sblock Stmt |
      Sexit Natural |
      Sswitch Bool Expr [(Integer, Natural)] Natural |
      Sreturn (Maybe Expr) |
      Slabel Label Stmt |
      Sgoto Label

data CMinorFunction

type CMinorProgram = Program (FunDef CMinorFunction) ()
