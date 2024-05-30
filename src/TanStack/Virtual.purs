module TanStack.Virtual where

import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, EffectFn2)
import Web.HTML (HTMLElement)

data Virtualizer

type VirtualizerOptions =
  { count ∷ Int
  , getScrollElement ∷ EffectFn1 Unit (Nullable HTMLElement)
  , estimateSize ∷ Int → Int
  , onChange ∷ EffectFn2 Virtualizer Boolean Unit
  }

type VirtualItem =
  { key ∷ String
  , index ∷ Int
  , start ∷ Int
  , end ∷ Int
  , size ∷ Int
  }

foreign import mkVirtualizer ∷ VirtualizerOptions → Virtualizer

foreign import getTotalSize ∷ Virtualizer → Int

foreign import getVirtualItems ∷ Virtualizer → Array VirtualItem

foreign import _didMount ∷ Virtualizer → Effect Unit
foreign import _willUpdate ∷ Virtualizer → Effect Unit
