module Program where

data Positive = XI Positive | XO Positive | XH

type Ident = Positive

type PtrOfs = Integer

newtype Error = Error String

type Res a = Either Error a

------------------------------------------
-- Integer
------------------------------------------

data Comparison
  = Ceq | Cne | Clt | Cle | Cgt | Cge

------------------------------------------
-- Memory
------------------------------------------

data MemoryChunk
  = MInt8Signed |
    MInt8Unsigned |
    Mint16Signed |
    MInt16UnSigned |
    MInt32 |
    MInt64 |
    MFloat32 |
    MFloat64 |
    Many32 |
    Many64

------------------------------------------
-- Program
------------------------------------------

data Typ
  = TInt |
    TFloat |
    TLong |
    TSingle |
    TAny32 |
    TAny64

data CallingConvention = CallingConvention
  {
    cc_varag :: Bool,
    cc_unproto :: Bool,
    cc_structure :: Bool
  }

data Signature = Signature
  {
    sig_args :: [Typ],
    sig_res :: Maybe Typ,
    sig_cc :: CallingConvention
  }

data InitData
  = InitInt8 Int |
    InitInt16 Int |
    InitInt32 Int |
    InitInt64 Int |
    InitFloat32 Float |
    InitFloat64 Float |
    InitSpace Integer |
    InitAddrof Ident PtrOfs

data GlobVar v = GlobVar
    {
        gvar_info :: v,
        gvar_init :: [InitData],
        gvar_readonly :: Bool,
        gvar_volatile :: Bool
    }

data GlobalDef f v =
    Gfun f
    | Gvar (GlobVar v)

data Program f v = Program
    {
        prog_defs :: [(Ident, Maybe (GlobalDef f v))],
        prog_public :: [Ident],
        prog_main :: Ident
    }

data ExternalFunction 

data FunDef f = Internal f | External ExternalFunction

data BuiltinArg a
  = BA a |
    BAInt Int |
    BALong Int |
    BAFloat Float |
    BASingle Float |
    BALoadStack MemoryChunk PtrOfs |
    BAAddrStack PtrOfs |
    BALoadGlobal MemoryChunk Ident PtrOfs |
    BAAddrGlobal Ident PtrOfs |
    BASplitLong (BuiltinArg a) (BuiltinArg a)

data BuiltinRes a
  = BR a |
    BRNone |
    BRSplitLong (BuiltinRes a) (BuiltinRes a)


