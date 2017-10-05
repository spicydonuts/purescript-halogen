module Halogen.Aff.Util
  ( awaitReady
  , awaitLoad
  , awaitElement
  , awaitBody
  , selectElement
  , runHalogenAff
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, Canceler(..), makeAff, nonCanceler, runAff_)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (readystatechange)
import DOM.HTML.Types (HTMLElement, htmlDocumentToEventTarget, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import Data.Either (Either(..), either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), maybe)
import Halogen.Aff.Effects (HalogenEffects)
import Unsafe.Coerce (unsafeCoerce)

documentReadyState :: forall eff. Eff (dom :: DOM | eff) String
documentReadyState = do
  doc <- document =<< window
  pure (unsafeCoerce doc).readyState

awaitReadyState :: forall eff. String -> Aff (dom :: DOM | eff) Unit
awaitReadyState state = isState <|> awaitState
  where
    isState = makeAff \cb -> do
      drs <- documentReadyState
      if drs == state
        then cb (Right unit)
        else cb (Left $ error "Not ready yet")
      pure nonCanceler
    awaitState = makeAff \cb -> do
      et <- htmlDocumentToEventTarget <$> (document =<< window)
      let listener = eventListener \_ -> do
            removeEventListener readystatechange listener false et
            runAff_ cb (awaitReadyState state)
      addEventListener readystatechange listener false et
      pure $ Canceler \_ -> liftEff $ removeEventListener readystatechange listener false et

-- | Waits for the document to become interactive.
awaitReady :: forall eff. Aff (dom :: DOM | eff) Unit
awaitReady = awaitReadyState "interactive"

-- | Waits for the document to load.
awaitLoad :: forall eff. Aff (dom :: DOM | eff) Unit
awaitLoad = awaitReadyState "complete"

-- | Waits for the document to be ready and then finds the given element.
awaitElement :: forall eff. QuerySelector -> Aff (dom :: DOM | eff) HTMLElement
awaitElement query = do
  awaitReady
  body <- selectElement query
  maybe (throwError $ error "Could not find body") pure body

-- | Waits for the document to be ready and then finds the `body` element.
awaitBody :: forall eff. Aff (dom :: DOM | eff) HTMLElement
awaitBody = awaitElement (QuerySelector "body")

-- | Tries to find an element in the document.
selectElement
  :: forall eff
   . QuerySelector
  -> Aff (dom :: DOM | eff) (Maybe HTMLElement)
selectElement query = do
  mel <- liftEff $
    ((querySelector query <<< htmlDocumentToParentNode <=< document) =<< window)
  pure case mel of
    Nothing -> Nothing
    Just el -> either (const Nothing) Just $ runExcept $ readHTMLElement (toForeign el)

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runHalogenAff
  :: forall eff x
   . Aff (HalogenEffects eff) x
  -> Eff (HalogenEffects eff) Unit
runHalogenAff = runAff_ (either throwException (const (pure unit)))
