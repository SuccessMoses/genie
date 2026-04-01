module CMinorSel where

import GHC.Natural (Natural)
import Program (Ident, MemoryChunk, Signature, ExternalFunction)
import CMinor (Label)

--------------------------------------------
-- x86
--------------------------------------------

data Operation
  = OMove |
    OIntConst Int |
    OLongConst Int |
    OFLoatConst Float |
    OSingleConst Float |
    OIndirectSymbol Ident |
    OCast8Signed |
    OCast8Unsigned |
    OCast16Signed |
    OCast16Unsigned |
    ONeg |
    OSub |
    OMul |
    OMulImm Int |
    OMulHs |
    OMulHu |
    ODiv |
    ODivU |
    OMod |
    OModU |
    OAnd |
    OAndImm Int |
    OOr |
    OOrImm Int |
    OXor |
    OXorImm Int |
    ONot |
    OShl |
    OShlImm Int |
    OShr |
    OshrImm Int |
    OshrXImm Int |
    OshrU |
    OShrUImm Int |
    ORoRImm Int |
    OshLDimm Int |
    OLea Addressing |
    OMakeLong |
    OLowLong |
    OHighLong |
    OCast32Signed |
    OCast32Unsigned |
    ONegl |
    OAddLImm Int |
    OSubl |
    OMull |
    OMullImm Int |
    OMullHs |
    OMullHu |
    ODivl |
    ODivlu |
    OModl |
    OModlu |
    OAndl |
    OAndLImm Int |
    OOrl |
    OOrLImm Int |
    OXorl |
    OXorlImm Int |
    ONotl |
    OShll |
    OShllImm Int |
    Oshrl |
    OshrlImm Int |
    Oshrl |
    OShrlImm Int |
    OshrrxlImm Int |
    Oshrlu |
    OshrluImm Int |
    OrOrlImm Int |
    OLeal Addressing |
    







--------------------------------------------
-- CMinorSel
--------------------------------------------

data Expr
  = EVar Ident |
    EOp Operation ExprList |
    ELoad MemoryChunk Addressing ExprList |
    ECondition CondExpr Expr Expr |
    ELet Expr Expr |
    ELetVar Natural |
    EBuiltin ExternalFunction ExprList |
    EExternal Ident Signature ExprList

data ExprList
  = ENil |
    ECons Expr ExprList ExprList

data CondExpr
  = CECond Condition ExprList |
    CECondition CondExpr CondExpr CondExpr |
    CELet Expr CondExpr

data ExitExpr
  = XEexit Natural |
    XEJumptable Expr [Natural] |
    XECondition CondExpr ExitExpr ExitExpr |
    XELet Expr ExitExpr

data Stmt
  = Sskip |
    Sassign Ident Expr |
    Sstore MemoryChunk Addressing ExprList Expr |
    Scall (Maybe Ident) Signature (Either Expr Ident) ExprList |
    STailCall Signature (Either Expr Ident) ExprList |
    SBuiltIn (BuiltinRes Ident) ExternalFunction [BuiltinArg Expr] |
    Sseq Stmt Stmt |
    SIfThenElse CondExpr Stmt Stmt |
    SLoop Stmt |
    SBlock Stmt |
    SExit Natural |
    SSwitch ExitExpr |
    SReturn (Maybe Expr) |
    SLabel Label Stmt |
    SGoto Label
