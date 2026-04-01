module Selection where

import Program

-------------------------
-- CMinorSel
-------------------------

data CMinorSelFunction

type CMinorSelProgram = Program (FunDef CMinorSelFunction) ()

-----------------------------
-- CMinor --> CMinorSel
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

selFunction :: PTree GlobalDef -> HelperFunctions -> CMinorFunction -> CMinorSelFunction

-- selProgram :: CMinorProgram -> Res CMinorSelProgram
-- selProgram p = myF p


