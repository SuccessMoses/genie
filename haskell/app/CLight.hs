module CLight where

import CSyntax (Typ, UnaryOperation, BinaryOperation, TypeList)
import Program (Ident, ExternalFunction)

data Expr
  = EConstInt Int Typ |
    EConstFloat Float Typ |
    EConstSingle Float Typ |
    EConstLong Int Typ |
    EVar Ident Typ |
    ETempVar Ident Typ |
    EDeref Expr Typ |
    EAddrOf Expr Typ |
    EUnOP UnaryOperation Expr Typ |
    EBinOp BinaryOperation Expr Expr Typ |
    ECast Expr Typ |
    EField Expr Ident Typ |
    ESizeOf Typ Typ |
    EAlignOf Typ Typ

type Label = Ident

data Statement
  = Sskip |
    SAssign Expr Expr |
    SSet Ident Expr |
    SCall (Maybe Ident) Expr [Expr] |
    SBuiltin (Maybe Ident) ExternalFunction TypeList [Expr] |
    SSequence Statement Statement |
    SIfThenElse Expr Statement Statement |
    SLoop Statement Statement |
    SBreak |
    SContinue |
    SReturn |
    SSwitch Expr LabelledStatements |
    SLabel Label Statement |
    SGoto Label

data LabelledStatements = LSNil | LSCons [Integer] Statement LabelledStatements