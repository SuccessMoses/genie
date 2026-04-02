module Selection where

import Program
import qualified CMinor
import CMinorSel

-------------------------
-- CMinorSel
-------------------------

data CMinorSelFunction

type CMinorSelProgram = Program (FunDef CMinorSelFunction) ()

-----------------------------
-- PTree
-----------------------------

data PTree a
  = Leaf |
    Node (PTree a) (Maybe a) (PTree a)

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

-----------------------------
-- CMinor --> CMinorSel
-----------------------------

condExprOfExpr :: Expr -> CondExpr
condExprOfExpr (EOp (OCmp c) el) = CECond c el
condExprOfExpr (ECondition a b c) = CECondition a (condExprOfExpr b) (condExprOfExpr c)
condExprOfExpr (ELet a b) = CELet a (condExprOfExpr b)
condExprOfExpr e = CECond (Ccompimm Cne 0) (ECons e ENil)

-- fixme: define `addressing`.
load :: MemoryChunk -> Expr -> Expr
load chunk e = ELoad chunk mode args
  where
    args = ECons e ENil
    mode = Aindexed 0

-- fixme: define `addressing`.
store :: MemoryChunk -> Expr -> Expr -> Stmt
store chunk e1 = Sstore chunk mode args
  where
    args = ECons e1 ENil
    mode = Aindexed 0

-- fixme: define `longconst`, `addrsymbol` and `addrstack`.
selConstant :: CMinor.Constant -> Expr
selConstant (CMinor.OIntConst n) = EOp (OIntConst n) ENil
selConstant (CMinor.OFloatConst f') = EOp (OFloatConst f') ENil
selConstant (CMinor.OSingleConst f') = EOp (OSingleConst f') ENil
selConstant (CMinor.OLongConst n) = EOp (OLongConst n) ENil
selConstant (CMinor.OAddRSymbol id' ofs) = EOp (OLea (Aglobal id' ofs)) ENil
selConstant (CMinor.OAddRStack ofs) = EOp (OLea (Ainstack ofs)) ENil

-- selUnOp :: CMinor.UnaryOperation -> Expr -> Expr
-- selUnOp (CMinor.OCast8Unsigned) e = EOp OCast8Unsigned (ECons e ENil)
-- selUnOp (CMinor.OCast8Signed) e = EOp OCast8Signed (ECons e ENil)
-- selUnOp (CMinor.OCast16Unsigned) e = EOp OCast16Unsigned (ECons e ENil)
-- selUnOp (CMinor.OCast16Signed) e = EOp OCast16Signed (ECons e ENil)
-- selUnOp (CMinor.ONegInt) e = EOp ONeg (ECons e ENil)
-- selUnOp (CMinor.ONotInt) e = EOp ONot (ECons e ENil)
-- selUnOp (CMinor.ONegF) e = EOp ONegf (ECons e ENil)
-- selUnOp (CMinor.OAbsF) e = EOp OAbsf (ECons e ENil)
-- selUnOp (CMinor.ONegFs) e = EOp ONegfs (ECons e ENil)
-- selUnOp (CMinor.OAbsFs) e = EOp OAbsfs (ECons e ENil)

-- selUnOp :: CMinor.UnaryOperation -> Expr -> Expr
-- selUnOp (CMinor.OCast8Unsigned) = 

-- selStmt :: CMinor.Stmt -> Res CMinorSel.Stmt
-- selStmt CMinor.Sskip = return CMinorSel.Sskip
-- selStmt (CMinor.Sassign id e) = return (CMinorSel.Sassign id (sel_expr e))

transfGlobVar :: Ident -> (Ident -> v -> Res w) -> GlobVar v -> Res (GlobVar w)
transfGlobVar i transfVar g = do
    info <- transfVar i (gvar_info g)
    return $ GlobVar 
      info 
      (gvar_init g) 
      (gvar_readonly g) 
      (gvar_volatile g)

transfGlobDefs :: 
  [(Ident, Maybe (GlobalDef a v))] ->
  (Ident -> a -> Res b) ->
  (Ident -> v -> Res w) -> 
  Res [(Ident, Maybe (GlobalDef b w))]
transfGlobDefs [] _ _ = return []
transfGlobDefs ((i, Nothing) : l) transf transv = do
    tl' <- transfGlobDefs l transf transv
    return $ (i, Nothing) : tl'
transfGlobDefs ((i, Just (Gfun f')) : l) transf transv = case transf i f' of
    Left msg -> Left msg -- fix me: construct proper error message.
    Right tf -> do
        tl' <- transfGlobDefs l transf transv
        return $ (i, Just $ Gfun tf) : tl'
transfGlobDefs ((i, Just (Gvar v)) : l) transf transv = case transfGlobVar i transv v of
    Left msg -> Left msg -- fix me: construct proper error message.
    Right tv -> do
        tl' <- transfGlobDefs l transf transv
        return $ (i, Just $ Gvar tv) : tl'

transformProgramPartial :: 
  Program a v ->
  (Ident -> a -> Res b) ->
  (Ident -> v -> Res w) ->
  Res (Program b w)
transformProgramPartial p transf transv = do
    l <- transfGlobDefs (prog_defs p) transf transv
    return $ Program l (prog_public p) (prog_main p)

myF :: Program a v -> (a -> Res b) -> Res (Program b v)
myF p transf = transformProgramPartial p (\_ f' -> transf f') (\_ v -> return v) -- fix variable shadowing then fixme!!

transfFunDefPartial :: (a -> Res b) -> FunDef a -> Res (FunDef b) -- FunDef is a functor
transfFunDefPartial transf (Internal f') = Internal <$> transf f'
transfFunDefPartial _ (External f') = return $ External f'



-- selFunction :: PTree GlobalDef -> HelperFunctions -> CMinorFunction -> CMinorSelFunction

-- selProgram :: CMinorProgram -> Res CMinorSelProgram
-- selProgram p = myF p


