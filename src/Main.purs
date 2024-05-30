module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, proxy, runStorybook)
import Halogen.Virtual as HV

stories ∷ ∀ m. MonadEffect m ⇒ Stories m
stories = Object.fromFoldable
  [ Tuple "basic-example" $ proxy HV.component
  ]

main ∷ Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Nothing
    }
