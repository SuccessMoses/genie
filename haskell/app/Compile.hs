module Compile where

type Ident = Positive

type PtrOfs = Integer

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

-------------------------
-- CMinorSel
-------------------------

data CMinorSelFunction

type CMinorSelProgram = Program (FunDef CMinorSelFunction) ()

--------------------------
-- CMinor
--------------------------

data CMinorFunction

type CMinorProgram = Program (FunDef CMinorFunction) ()

-----------------------------
-- CMinor --> CMinorSel
-----------------------------

data PTree a
  = Leaf |
    Node (PTree a) (Maybe a) (PTree a)

data Positive = XI Positive | XO Positive | XH

remove :: Positive -> PTree a -> PTree a
remove i m = case i of
    XH -> case m of
        Leaf -> Leaf
        Node Leaf _ Leaf -> Leaf
        Node l _ r -> Node l Nothing r
    XO ii -> case m of
        Leaf -> Leaf
        Node l Nothing Leaf -> case remove ii l of
            Leaf -> Leaf
            mm -> Node mm Nothing Leaf
        Node l o r -> Node (remove ii l) o r
    XI ii -> case m of
        Leaf -> Leaf
        Node Leaf Nothing r -> case remove ii r of
            Leaf -> Leaf
            mm -> Node Leaf Nothing mm
        Node l o r -> Node l o (remove ii r)

set :: Positive -> a -> PTree a -> PTree a
set i v m = case m of
    Leaf -> case i of
        XH -> Node Leaf (Just v) Leaf
        XO ii -> Node (set ii v Leaf) Nothing Leaf
        XI ii -> Node Leaf Nothing (set ii v Leaf)
    Node l o r -> case i of
        XH -> Node l (Just v) r
        XO ii -> Node (set ii v l) o r
        XI ii -> Node l o (set ii v r)

f :: PTree a -> (Positive, Maybe a) -> PTree a
f m kv = case snd kv of
    Just o -> set (fst kv) o m
    Nothing -> remove (fst kv) m

ofListOption :: [(Positive, Maybe a)] -> PTree a
ofListOption = foldl f Leaf

progDefMap :: Program f v -> PTree (GlobalDef f v)
progDefMap p = ofListOption (prog_defs p)

selProgram :: CMinorProgram -> Either CMinorSelProgram

