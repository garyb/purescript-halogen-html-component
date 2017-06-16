module Halogen.Component.HTML
  ( UnsafeHTMLString(..)
  , Query(..)
  , htmlComponent
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import DOM.HTML.Types (htmlElementToNode) as DOM
import DOM.Node.Node (appendChild, clone, parentNode, replaceChild) as DOM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Unsafe.Reference (unsafeRefEq)

-- | A newtype for a string that will be used as `innerHTML`. As the name
-- | suggests this can be unsafe if the HTML string is not from a trusted
-- | source, opening the door to XSS attacks, etc.
newtype UnsafeHTMLString = UnsafeHTMLString String

derive newtype instance eqUnsafeHTMLString ∷ Eq UnsafeHTMLString

data Query a = SetHTML (Either UnsafeHTMLString HTMLElement) a

-- | A component that encapsulates HTML from outside of Halogen.
htmlComponent
  ∷ ∀ m eff
  . MonadEff (dom ∷ DOM | eff) m
  ⇒ H.Component HH.HTML Query (Either UnsafeHTMLString HTMLElement) Void m
htmlComponent =
  H.lifecycleComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  render ∷ ∀ a. a → H.ComponentHTML Query
  render _ =
    HH.span
      [ HP.ref refLabel ]
      []

  eval ∷ Query ~> H.ComponentDSL (Either UnsafeHTMLString HTMLElement) Query Void m
  eval (SetHTML content next) = do
    oldContent ← H.get
    let
      needsUpdate = case oldContent, content of
        Left x, Left y → x == y
        Right x, Right y → unsafeRefEq x y
        _, _ → true
    when needsUpdate do
      H.put content
      H.getHTMLElementRef refLabel >>= traverse_ \container →
        liftEff $
          either
            (\(UnsafeHTMLString s) → setInnerHTML container s)
            (replaceContents container)
            content
    pure next

  refLabel ∷ H.RefLabel
  refLabel = H.RefLabel "container"

foreign import setInnerHTML ∷ ∀ eff. HTMLElement → String → Eff (dom ∷ DOM | eff) Unit

replaceContents ∷ ∀ eff. HTMLElement → HTMLElement → Eff (dom ∷ DOM | eff) Unit
replaceContents container contents = do
  let containerNode = DOM.htmlElementToNode container
  container' ← DOM.clone containerNode
  _ ← DOM.appendChild container' (DOM.htmlElementToNode contents)
  DOM.parentNode containerNode >>= traverse_ \parent →
    DOM.replaceChild parent containerNode container'
