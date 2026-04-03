module Integer where
import Program (Positive(..))

-----------------------------------------
-- Positive
-----------------------------------------

doublePositive' :: Positive -> Positive
doublePositive' = XO

predDouble :: Positive -> Positive
predDouble = id --fixme!! wrong defintion

pIter :: (a -> a) -> a -> Positive -> a
pIter f = iter
  where
    iter a n = case n of
        XH -> f a
        XO n' -> iter (iter a n') n'
        XI n' -> f $ iter (iter a n') n'

-----------------------------------------
-- Z
-----------------------------------------

data Z = Z0 | ZPos Positive | ZNeg Positive

one :: Z
one = ZPos XH

double :: Z -> Z
double Z0 = Z0
-- 2*(+p) = +(2*p)
double (ZPos p) = ZPos (XO p)
-- 2*(-p) = -(2*p)
double (ZNeg p) = ZNeg (XO p)

succDouble :: Z -> Z
-- (2*0) + 1
succDouble Z0 = ZPos XH
-- 2(+n) + 1 = +(2n + 1)
succDouble (ZPos p) = ZPos (XI p)
-- 2(-n) + 1 = -(2n - 1)
succDouble (ZNeg p) = ZNeg (predDouble p)

zIter :: Z -> (a -> a) -> a -> a
zIter Z0 _ a = a
zIter (ZPos p) f a = pIter f a p
zIter (ZNeg _) _ a = a

zOdd :: Z -> Bool
zOdd = const True -- fixme

zPred :: Z -> Z
zPred = id --fixme

zShiftIn :: Bool -> Z -> Z
zShiftIn = const id --fixme

zDiv2 :: Z -> Z
zDiv2 = id

zSignExt :: Z -> Z -> Z
zSignExt n = zIter (zPred n) (\rec x -> zShiftIn (zOdd x) (rec (zDiv2 x))) (\x -> if zOdd x then one else one)
