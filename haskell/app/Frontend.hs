module Frontend where

import Program (Ident, Positive (XH))
import Control.Monad.Trans.State

data Typ

data Generator = Generator
  {
    gen_next :: Ident,
    gen_trail :: [(Ident, Typ)]
  }

type Result g a = StateT g (Either String) a

-- fixme!! I am incorrect!!!!!!
pSucc :: Ident -> Ident
pSucc _ = XH

genSym :: Typ -> Result Generator ()
genSym ty = modify (\g -> Generator (pSucc (gen_next g)) ((gen_next g, ty) : gen_trail g))
