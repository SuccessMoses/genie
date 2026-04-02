module CMinorSel where

import GHC.Natural (Natural)
import Program
  ( Ident
  , MemoryChunk
  , Signature
  , ExternalFunction
  , Comparison
  , PtrOfs
  , BuiltinArg
  , BuiltinRes
  )

import CMinor (Label)

--------------------------------------------
-- x86
--------------------------------------------

data Addressing
    = Aindexed Int                        
    | Aindexed2 Int                       
    | Ascaled Int Int                   
    | Aindexed2scaled Int Int           
    | Aglobal Ident PtrOfs            
    | Abased Ident PtrOfs                
    | Abasedscaled Int Ident PtrOfs       
    | Ainstack PtrOfs                     


data Condition
    = Ccomp Comparison                    
    | Ccompu Comparison                   
    | Ccompimm Comparison Int             
    | Ccompuimm Comparison Int            
    | Ccompl Comparison                   
    | Ccomplu Comparison                  
    | Ccomplimm Comparison Int          
    | Ccompluimm Comparison Int      
    | Ccompf Comparison                   
    | Cnotcompf Comparison                
    | Ccompfs Comparison                  
    | Cnotcompfs Comparison               
    | Cmaskzero Int                       
    | Cmasknotzero Int              

data Operation
  = OMove
  | OIntConst Int
  | OLongConst Int
  | OFloatConst Float
  | OSingleConst Float
  | OIndirectSymbol Ident
  
  -- 32-bit integer arithmetic
  | OCast8Signed
  | OCast8Unsigned
  | OCast16Signed
  | OCast16Unsigned
  | ONeg
  | OSub
  | OMul
  | OMulImm Int
  | OMulhs
  | OMulhu
  | ODiv
  | ODivu
  | OMod
  | OModu
  | OAnd
  | OAndImm Int
  | OOr
  | OOrImm Int
  | OXor
  | OXorImm Int
  | ONot
  | OShl
  | OShlImm Int
  | OShr
  | OShrImm Int
  | OShrximm Int
  | OShru
  | OShruImm Int
  | ORorimm Int
  | OShldimm Int
  | OLea Addressing
  
  -- 64-bit integer arithmetic
  | OMakeLong
  | OLowLong
  | OHighLong
  | OCast32Signed
  | OCast32Unsigned
  | ONegl
  | OAddlimm Int
  | OSubl
  | OMull
  | OMullImm Int
  | OMullhs
  | OMullhu
  | ODivl
  | ODivlu
  | OModl
  | OModlu
  | OAndl
  | OAndlimm Int
  | OOrl
  | OOrlimm Int
  | OXorl
  | OXorlimm Int
  | ONotl
  | OShll
  | OShllimm Int
  | OShrl
  | OShrlimm Int
  | OShrxlimm Int
  | OShrlu
  | OShrluimm Int
  | ORorlimm Int
  | OLeal Addressing
  
  -- Floating-point arithmetic
  | ONegf
  | OAbsf
  | OAddf
  | OSubf
  | OMulf
  | ODivf
  | ONegfs
  | OAbsfs
  | OAddfs
  | OSubfs
  | OMulfs
  | ODivfs
  | OSingleoffloat
  | OFloatofsingle
  
  -- Conversions between int and float
  | OIntoffloat
  | OFloatofint
  | OIntofsingle
  | OSingleofint
  | OLongoffloat
  | OFloatoflong
  | OLongofsingle
  | OSingleoflong
  
  -- Boolean tests
  | OCmp Condition

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
    ECons Expr ExprList

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
