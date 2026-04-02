module CSyntax where

import Program (Positive, PtrOfs, CallingConvention, Ident, ExternalFunction)

-------------------------------
-- COp
-------------------------------

data UnaryOperation
  = ONotBool |
    ONotInt |
    ONeg |
    OAbsFloat

data BinaryOperation
  = OAdd |
    OSub |
    OMul |
    ODiv |
    OMod |
    OAnd |
    OOr |
    OXor |
    OShl |
    OShr |
    OEq |
    ONe |
    OLt |
    OGt |
    OLe |
    OGe 

data IncrOrDecr = Incr | Decr

-----------------------------------
-- Values
-----------------------------------

type Block = Positive

data Val
  = VUnDef |
    VInt Int |
    VLong  Int |
    VFloat Float |
    VSingle Float |
    VPtr Block PtrOfs

------------------------------------
-- Ctypes
------------------------------------

data Signedness = Signed | Unsigned

data IntSize = I8 | I16 | I32 | IBool

data FloatSize = F32 | F64

data Attr = Attr
  {
    attr_volatile :: Bool,
    attr_alignas :: Bool
  }

data Typ
  = TVoid |
    TInt IntSize Signedness Attr |
    TLong Signedness Attr |
    TFloat FloatSize Attr |
    TPointer Typ Attr |
    TArray Typ Integer Attr |
    TFunction TypeList Typ CallingConvention |
    TStruct Ident Attr |
    TUnion Ident Attr 

data TypeList = TNil | TCons Typ TypeList

-------------------------------------------------
-- CSyntax
-------------------------------------------------

data Expr
  = EVal Val Typ |
    EVar Ident Typ |
    EField Expr Ident Typ |
    EValOf Expr Typ |
    EDeref Expr Typ |
    EAddrOf Expr Typ |
    EUnOp UnaryOperation Expr Typ |
    EBinOp BinaryOperation Expr Expr Typ |
    ECast Expr Typ |
    ESeqAnd Expr Expr Typ |
    ESeqOr Expr Expr Typ |
    ECondition Expr Expr Expr Typ |
    ESizeOf Typ Typ |
    EAlignOf Typ Typ |
    EAssign Expr Expr Typ |
    EAssignOp BinaryOperation Expr Expr Typ Typ |
    EPostIncr IncrOrDecr Expr Typ |
    EComma Expr Expr Typ |
    ECall Expr ExprList Typ |
    EBuiltin ExternalFunction TypeList ExprList Typ |
    ELoc Block PtrOfs Typ |
    EParen Expr Typ Typ

data ExprList
  = ENil | ECons Expr ExprList
