module Frontend where

import Program (Ident, Positive (XH))
import qualified CSyntax
import CSyntax (Typ, Val(..), typeInt32s)
import CLight ( Expr(..), Statement(SBuiltin, Sskip, SSet, SCall) )
import Control.Monad.Trans.State ( StateT, get, put )
import Control.Monad.Except ( MonadError(throwError) )

data Generator = Generator
  {
    gen_next :: Ident,
    gen_trail :: [(Ident, Typ)]
  }

type Result g a = StateT g (Either String) a

type M a = Result Generator a

-- fixme!! I am incorrect!!!!!!
pSucc :: Ident -> Ident
pSucc _ = XH

genSym :: Typ -> M Ident
genSym ty = do
  g <- get
  let fresh = gen_next g
  put $ Generator (pSucc fresh) ((fresh, ty) : gen_trail g)
  return fresh

-------------------------------------
-- SimplExpr
------------------------------------

dummyExpr :: Expr
dummyExpr = EConstInt 0 typeInt32s

evalSimplExpr :: Expr -> Maybe Val
evalSimplExpr (EConstInt n _) = Just $ VInt n
evalSimplExpr (EConstFloat n _) = Just $ VFloat n
evalSimplExpr (EConstSingle n _) = Just $ VSingle n
evalSimplExpr (EConstLong n _) =  Just $ VLong n
evalSimplExpr (ECast b ty) = case evalSimplExpr b of
  Nothing -> Nothing
  Just v -> semCast v (typeOf b) ty ()
evalSimplExpr _ = Nothing

data SetDestination
  = SDBase Typ Typ Ident |
    SDCons Typ Typ Ident SetDestination

data Destination
  = ForVal |
    ForEffects |
    ForSet SetDestination

doSet :: SetDestination -> Expr -> [Statement]
doSet (SDBase tycast _ tmp) a = [SSet tmp (ECast a tycast)]
doSet (SDCons tycast ty tmp sd') a = SSet tmp (ECast a tycast) : doSet sd' (ETempVar tmp ty)

finish :: Destination -> [Statement] -> Expr -> ([Statement], Expr)
finish ForVal sl a = (sl, a)
finish ForEffects sl a = (sl, a)
finish (ForSet sd) sl a = (sl ++ doSet sd a, a)

translExpr :: Destination -> CSyntax.Expr -> M ([Statement], Expr)
translExpr _ (CSyntax.ELoc {}) = throwError "" --fixme: proper error message
translExpr dst (CSyntax.EVar x ty) = return $ finish dst [] (EVar x ty)
translExpr dst (CSyntax.EDeref r ty) = do
  (sl, a) <- translExpr ForVal r
  return $ finish dst sl (EDeref a ty)
translExpr dst (CSyntax.EField r f ty) = do
  (sl, a) <- translExpr ForVal r
  return $ finish dst sl (EField a f ty)
translExpr dst (CSyntax.EVal (VInt n) ty) = return $ finish dst [] (EConstInt n ty)
translExpr dst (CSyntax.EVal (VFloat n) ty) =  return $ finish dst [] (EConstFloat n ty)
translExpr dst (CSyntax.EVal (VSingle n) ty) = return $ finish dst [] (EConstSingle n ty)
translExpr dst (CSyntax.EVal (VLong n) ty) =  return $ finish dst [] (EConstLong n ty)
translExpr dst (CSyntax.EVal _ _) = throwError "" --fixme: proper error message
translExpr dst (CSyntax.ESizeOf ty' ty) = return $ finish dst [] (ESizeOf ty' ty)
translExpr dst (CSyntax.EAlignOf ty' ty) = return $ finish dst [] (EAlignOf ty' ty)
translExpr dst (CSyntax.EValOf l ty) = do
  (sl1, a1) <- translExpr ForVal l
  (sl2, a2) <- translExprValOf (CSyntax.typeOf l) a1
  return (finish dst (sl1 ++ sl2) a2)
translExpr dst (CSyntax.EAddrOf l ty) = do
  (sl, a) <- translExpr ForVal l
  return (finish dst sl (EAddrOf a ty))
translExpr dst (CSyntax.EUnOp op r1 ty) = do
  (sl, a) <- translExpr ForVal r1
  return $ finish dst sl (EUnOP op a ty)
translExpr dst (CSyntax.EBinOp op r1 r2 ty) = do
  (sl1, a1) <- translExpr ForVal r1
  (sl2, a2) <- translExpr ForVal r2
  return $ finish dst (sl1 ++ sl2) (EBinOp op a1 a2 ty)
translExpr dst (CSyntax.ECast r1 ty) = do
  (sl1, a1) <- translExpr ForVal r1
  return $ finish dst sl1 (ECast a1 ty)
translExpr dst (CSyntax.ESeqAnd r1 r2 ty) = do
  (sl1, a1) <- translExpr ForVal r1
  case dst of
    ForVal -> do
      t <- genSym ty
      (sl2, a2) <- translExpr (ForSet (sdSeqBoolVal t ty)) r2
      return
        (
          sl1 ++ [makeIf a1 (makeSeq sl2) (SSet t (EConstInt 0 ty))],
          ETempVar t ty
        )
    ForEffects -> do
      (sl2, a2) <- translExpr ForEffects r2
      return (sl1 ++ [makeIf a1 (makeSeq sl2) Sskip], dummyExpr)
    ForSet sd -> do
      (sl2, _) <- translExpr (ForSet $ sdSeqBoolSet ty sd) r2
      return
        (
          sl1 ++ [makeIf a1 (makeSeq sl2) (makeSeq $ doSet ds (EConstInt 0 ty))],
          dummyExpr
        )
translExpr dst (CSyntax.ESeqOr r1 r2 ty) = do
  (sl1, a1) <- translExpr ForVal r1
  case dst of
    ForVal -> do
      t <- genSym ty
      (sl2, a2) <- translExpr (ForSet $ sdSeqBoolVal t ty) r2
      return
        (
          sl1 ++ [makeIf a1 (SSet t $ EConstInt 1 ty) (makeSeq sl2)],
          ETempVar t ty
        )
    ForEffects -> do
      (sl2, a2) <- translExpr ForEffects r2
      return (sl1 ++ [makeIf a1 SSkip $ makeSeq sl2], dummyExpr)
    ForSet sd -> do
      (sl2, a2) <- translExpr (ForSet $ sdSeqBoolSet ty sd) r2
      return
        (
          sl1 ++ [makeIf a1 (makeSeq $ doSet sd (EConstInt 1 ty)) (makeSeq sl2)],
          dummyExpr
        )
translExpr dst (CSyntax.ECondition r1 r2 r3 ty) = do
  (sl1, a1) <- translExpr ForVal r1
  case dst of
    ForVal -> do
      t <- genSym ty
      (sl2, a2) <- translExpr (ForSet $ SDBase ty ty t) r2
      (sl3, a3) <- translExpr (ForSet $ SDBase ty ty t) r3
      return
        (
          sl1 ++ [makeIf a1 (makeSeq sl2) (makeSeq sl3)],
          ETempVar t ty
        )
    ForEffects -> do
      (sl2, a2) <- translExpr ForEffects r2
      (sl3, a3) <- translExpr ForEffects r3
      return
        (
          sl1 ++ [makeIf a1 (makeSeq sl2) (makeSeq sl3)],
          dummyExpr
        )
    ForSet sd -> do
      t <- genSym ty
      (sl2, a2) <- translExpr (ForSet $ SDCons ty ty t sd) r2
      (sl3, a3) <- translExpr (ForSet $ SDCons ty ty t sd) r3
      return
        (
          sl1 ++ [makeIf a1 (makeSeq sl2) (makeSeq sl3)],
          dummyExpr
        )
translExpr dst (CSyntax.EAssign l1 r2 ty) = do
  (sl1, a1) <- translExpr ForVal l1
  (sl2, a2) <- translExpr ForVal r2
  let ty1 = CSyntax.typeOf l1
  let ty2 = CSyntax.typeOf r2
  case dst of
    ForVal -> do
      t <- genSym ty1
      return $
        finish dst
          (sl1 ++ sl2 ++ [SSet t (ECast a2 ty1), makeAssign a1 (ETempVar t ty1)])
          (ETempVar t ty1)
    ForSet _ -> do
      t <- genSym ty1
      return $
        finish dst
          (sl1 ++ sl2 ++ [SSet t (ECast a2 ty1), makeAssign a1 (ETempVar t ty1)])
          (ETempVar t ty1)
    ForEffects -> return (sl1 ++ sl2 ++ [makeAssign a1 a2], dummyExpr)
translExpr dst (CSyntax.EAssignOp op l1 r2  tyres ty) = do
  let ty1 = CSyntax.typeOf l1
  (sl1, a1) <- translExpr ForVal l1
  (sl2, a2) <- translExpr ForVal r2
  (sl3, a3) <- translValOf ty1 a1
  case dst of
    ForVal -> do
      t <- genSym ty1
      return $
        finish dst
          (sl1 ++ sl2 ++ sl3 ++
            [SSet t (ECast (EBinOp op a3 a2 tyres) ty1), makeAssign a1 (ETempVar t ty1)])
          (ETempVar t ty1)
    ForSet _ -> do
      t <- genSym ty1
      return $
        finish dst
          (sl1 ++ sl2 ++ sl3 ++
            [SSet t (ECast (EBinOp op a3 a2 tyres) ty1), makeAssign a1 (ETempVar t ty1)])
          (ETempVar t ty1)
    ForEffects ->
      return (sl1 ++ sl2 ++ sl3 ++ [makeAssign a1 (EBinOp op a3 a2 tyres)], dummyExpr)
translExpr dst (CSyntax.EPostIncr id' l1 ty) = do
  let ty1 = CSyntax.typeOf l1
  case dst of
    ForVal -> do
      t <- genSym ty1
      return $
        finish dst
          (sl1 ++ [makeSet t a1, makeAssign a1 $ translIncrDecr id' (ETempVar t ty1) ty1])
          (ETempVar t ty1)
    ForSet _ -> do
      t <-  genSym ty1
      return $
        finish dst
          (sl1 ++ [makeSet t a1, makeAssign a1 $ translIncrDecr id' (ETempVar t ty1) ty1])
          (ETempVar t ty1)
    ForEffects -> do
      (sl2, a2) <- translValOf ty1 a1
      return (sl1 ++ sl2 ++ [makeAssign a1 $ translIncrDecr id' a2 ty1], dummyExpr)
translExpr dst (CSyntax.EComma r1 r2 ty) = do
  (sl1, a1) <- translExpr ForEffects r1
  (sl2, a2) <- translExpr dst r2
  return (sl1 ++ sl2, a2)
translExpr dst (CSyntax.ECall r1 rl2 ty) = do
  (sl1, a1) <- translExpr ForVal r1
  (sl2, al2) <- translExprList rl2
  case dst of
    ForVal -> do
      t <- genSym ty
      return $
        finish dst
          (sl1 ++ sl2 ++ [SCall (Just t) a1 al2])
          (ETempVar t ty)
    ForSet _ -> do
      t <- genSym ty
      return $
        finish dst
          (sl1 ++ sl2 ++ [SCall (Just t) a1 al2])
          (ETempVar t ty)
    ForEffects -> return (sl1 ++ sl2 ++ [SCall Nothing a1 al2], dummyExpr)
translExpr dst (CSyntax.EBuiltin ef tyargs rl ty) = do
  (sl, al) <- translExprList rl
  case dst of
    ForVal -> do
      t <- genSym ty
      return $
        finish dst (sl ++ [SBuiltIn (Just t) ef tyargs al]) (ETempVar t ty)
    ForSet _ -> do
      t <- genSym ty
      return $
        finish dst (sl ++ [SBuiltin (Just t) ef tyargs al]) (ETempVar t ty)
    ForEffects -> return (sl ++ [SBuiltIn Nothing ef tyargs al], dummyExpr)
translExpr _ (CSyntax.EParen {}) = throwError "" --fixme: add proper error message.

translExprList :: CSyntax.ExprList -> M ([Statement], [Expr])
translExprList CSyntax.ENil = return ([], [])
translExprList (CSyntax.ECons r1 rl2) = do
  (sl1, a1) <- translExpr ForVal r1
  (sl2, al2) <- translExprList rl2
  return (sl1 ++ sl2, a1 : al2)




