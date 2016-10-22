module Data.Lens.Iso.Coproduct where

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct(..))
import Data.Newtype (unwrap)
import Data.Lens.Iso (Iso, iso)

_Coproduct
  :: forall f g h i a b
   . Iso
      (Coproduct f g a)
      (Coproduct h i b)
      (Either (f a) (g a))
      (Either (h b) (i b))
_Coproduct = iso unwrap Coproduct
