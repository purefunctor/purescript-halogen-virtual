module Halogen.Virtual where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Class (class MonadEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import TanStack.Virtual
  ( VirtualItem
  , Virtualizer
  , _didMount
  , _willUpdate
  , getTotalSize
  , getVirtualItems
  , mkVirtualizer
  )
import Web.HTML (HTMLElement)

type State =
  { virtualizer ∷ Maybe Virtualizer
  }

initialState ∷ ∀ i. i → State
initialState _ =
  { virtualizer: Nothing
  }

containerLabel ∷ RefLabel
containerLabel = RefLabel "virtual-container"

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state =
  let
    height ∷ Int
    height = Maybe.fromMaybe 0 (getTotalSize <$> state.virtualizer)

    virtualItems ∷ Array VirtualItem
    virtualItems = Maybe.fromMaybe [] (getVirtualItems <$> state.virtualizer)

    items ∷ Array (HH.ComponentHTML Action () m)
    items = virtualItems <#> \virtualItem →
      HH.div
        [ HP.style $
            "position: absolute; top: 0; left: 0; width: 100%; height: "
              <> show virtualItem.size
              <> "px; transform: translateY("
              <> show virtualItem.start
              <> "px);"
        ]
        [ HH.span
            [ HP.style "display: flex; align-items: center; height: 100%;" ]
            [ HH.text $ virtualItem.key ]
        ]
  in
    HH.div
      [ HP.ref containerLabel
      , HP.style
          "height: 400px; overflow: auto; border-color: rgba(0,0,0,0.1); border-width: 1px; border-style: solid;"
      ]
      [ HH.div
          [ HP.style $ "height: " <> show height <>
              "px; width: 100%; position: relative;"
          ]
          items
      ]

data Action = Initialize | Render

handleAction
  ∷ ∀ o m. MonadEffect m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize → do
    { emitter, listener } ← H.liftEffect $ HS.create
    _ ← H.subscribe emitter

    mContainerEl ← H.getHTMLElementRef containerLabel
    let
      getScrollElement ∷ EffectFn1 Unit (Nullable HTMLElement)
      getScrollElement = mkEffectFn1 \_ → do
        pure $ case mContainerEl of
          Just containerEl →
            Nullable.notNull containerEl
          Nothing →
            Nullable.null

      onChange ∷ EffectFn2 Virtualizer Boolean Unit
      onChange = mkEffectFn2 \_ _ →
        HS.notify listener Render

      virtualizer ∷ Virtualizer
      virtualizer = mkVirtualizer
        { count: 100
        , getScrollElement
        , estimateSize: \_ → 30
        , onChange
        }

    H.put { virtualizer: Just virtualizer }
    H.liftEffect do
      _didMount virtualizer
      _willUpdate virtualizer

  Render → do
    H.modify_ \{ virtualizer } → { virtualizer }

component ∷ ∀ q i o m. MonadEffect m ⇒ H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
