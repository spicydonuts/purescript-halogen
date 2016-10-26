module Halogen.VirtualDOM.Driver
  ( Driver
  , runUI
  ) where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine (($$))
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, runAff, forkAff)
import Control.Monad.Aff.AVar (AVAR, AVar, putVar, takeVar, modifyVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Fork (fork)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (sequential, parallel)

import Data.Lazy (force)
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_, sequence)
import Data.Tuple (Tuple(..))

import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.Node.Node (appendChild)

import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox, unOrdBox)
import Halogen.VirtualDOM.Driver.State (DriverStateX, DriverState(..), unDriverStateX, initDriverState)
import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core (HTML)
import Halogen.VirtualDOM.Renderer (renderHTML)
import Halogen.VirtualDOM.Internal as V
import Halogen.Query.ChildQuery (ChildQuery, unChildQuery)
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))

-- | Type alias for driver functions generated by `runUI` - a driver takes an
-- | input of the query algebra (`f`) and returns an `Aff` that returns when
-- | query has been fulfilled.
type Driver f eff = f ~> Aff (HalogenEffects eff)

type DSL s f g eff p o = HalogenF s f g p o (Aff (HalogenEffects eff))

type LifecycleHandlers eff =
  { initializers :: List (Aff (HalogenEffects eff) Unit)
  , finalizers :: List (Aff (HalogenEffects eff) Unit)
  }

addInitializer
  :: forall g eff
   . Ref (LifecycleHandlers eff)
  -> DriverStateX g eff
  -> Aff (HalogenEffects eff) Unit
addInitializer ref dsx =
  for_ (unDriverStateX (\st -> evalF st.selfRef <$> st.component.initializer) dsx) \i ->
    liftEff $ modifyRef ref (\lchs ->
      { initializers: i : lchs.initializers
      , finalizers: lchs.finalizers
      })

addFinalizer
  :: forall f eff
   . Ref (LifecycleHandlers eff)
  -> DriverStateX f eff
  -> Aff (HalogenEffects eff) Unit
addFinalizer ref =
  unDriverStateX \st -> do
    for_ (evalF st.selfRef <$> st.component.finalizer) \f ->
      liftEff $ modifyRef ref (\lchs ->
        { initializers: lchs.initializers
        , finalizers: f : lchs.finalizers
        })
    for_ st.children (addFinalizer ref <=< peekVar)

handleLifecycle
  :: forall eff r
   . (Ref (LifecycleHandlers eff) -> Aff (HalogenEffects eff) r)
  -> Aff (HalogenEffects eff) r
handleLifecycle f = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  result <- f lchs
  { initializers, finalizers } <- liftEff $ readRef lchs
  sequence $ L.reverse initializers
  sequence finalizers
  pure result

-- | This function is the main entry point for a Halogen based UI, taking a root
-- | component, initial state, and HTML element to attach the rendered component
-- | to.
-- |
-- | The returned "driver" function can be used to send actions and requests
-- | into the component hierarchy, allowing the outside world to communicate
-- | with the UI.
runUI
  :: forall f eff o
   . Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (Driver f eff)
runUI component element = _.driver <$> do
  fresh <- liftEff $ newRef 0
  handleLifecycle \lchs ->
    runComponent (const (pure unit)) fresh lchs component
      >>= peekVar
      >>= unDriverStateX \st -> do
        liftEff $ appendChild (htmlElementToNode st.node) (htmlElementToNode element)
        -- The record here is a hack around a skolem escape issue. If the typing
        -- rules for records change so this no longer works it may also be
        -- fixable with copious type annotations.
        pure { driver: evalF st.selfRef }

runComponent
  :: forall f eff o
   . (o -> Aff (HalogenEffects eff) Unit)
  -> Ref Int
  -> Ref (LifecycleHandlers eff)
  -> Component HTML f o (Aff (HalogenEffects eff))
  -> Aff (HalogenEffects eff) (AVar (DriverStateX f eff))
runComponent handler fresh lchs = unComponent \component -> do
  keyId <- liftEff $ readRef fresh
  liftEff $ modifyRef fresh (_ + 1)
  var <- initDriverState component handler keyId fresh
  unDriverStateX (render lchs <<< _.selfRef) =<< peekVar var
  addInitializer lchs =<< peekVar var
  pure var

eval
  :: forall s f g eff p o
   . AVar (DriverState s f g eff p o)
  -> DSL s f g eff p o
  ~> Aff (HalogenEffects eff)
eval ref = case _ of
  GetState k -> do
    DriverState { state } <- peekVar ref
    pure (k state)
  ModifyState f next -> do
    modifyVar (\(DriverState st) -> DriverState (st { state = f st.state })) ref
    handleLifecycle \lchs -> render lchs ref
    pure next
  Subscribe es next -> do
    let consumer = forever (lift <<< evalF ref =<< CR.await)
    forkAff $ CR.runProcess (unwrap es $$ consumer)
    pure next
  Lift aff ->
    aff
  Halt msg ->
    throwError (error msg)
  GetSlots k -> do
    DriverState { children } <- peekVar ref
    pure $ k $ map unOrdBox $ M.keys children
  CheckSlot p k -> do
    DriverState { mkOrdBox, children } <- peekVar ref
    pure $ k $ M.member (mkOrdBox p) children
  ChildQuery cq ->
    evalChildQuery ref cq
  Raise o a -> do
    DriverState { handler } <- peekVar ref
    handler o
    pure a
  Par (HalogenAp p) ->
    sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
  Fork f ->
    FF.unFork (\(FF.ForkF fx k) →
      k <<< map unsafeCoerceAff <$> fork (evalM ref fx)) f

evalChildQuery
  :: forall s f g eff p o
   . AVar (DriverState s f g eff p o)
  -> ChildQuery g (Aff (HalogenEffects eff)) p
  ~> Aff (HalogenEffects eff)
evalChildQuery ref = unChildQuery \p k -> do
  DriverState st <- peekVar ref
  case M.lookup (st.mkOrdBox p) st.children of
    Just var -> do
      dsx <- peekVar var
      k (unDriverStateX (\ds -> evalF ds.selfRef) dsx)
    Nothing -> throwError (error "Slot lookup failed for child query")

evalF
  :: forall s f g eff p o
   . AVar (DriverState s f g eff p o)
  -> f
  ~> Aff (HalogenEffects eff)
evalF ref q = do
  DriverState st <- peekVar ref
  case st.component.eval q of
    HalogenM fx -> foldFree (eval ref) fx

evalM
  :: forall s f g eff p o
   . AVar (DriverState s f g eff p o)
  -> HalogenM s f g p o (Aff (HalogenEffects eff))
  ~> Aff (HalogenEffects eff)
evalM ref (HalogenM q) = foldFree (eval ref) q

render
  :: forall s f g eff p o
   . Ref (LifecycleHandlers eff)
  -> AVar (DriverState s f g eff p o)
  -> Aff (HalogenEffects eff) Unit
render lchs var = takeVar var >>= \(DriverState ds) -> do
  childrenVar <- liftEff $ newRef M.empty
  oldChildren <- liftEff $ newRef ds.children
  let selfEval = evalF ds.selfRef
  vtree' <-
    renderHTML
      (handleAff <<< selfEval)
      (renderChild selfEval ds.fresh ds.mkOrdBox oldChildren childrenVar lchs)
      (ds.component.render ds.state)
  node' <- liftEff $ V.patch (V.diff ds.vtree vtree') ds.node
  children <- liftEff $ readRef childrenVar
  liftEff (readRef oldChildren) >>= traverse_ (addFinalizer lchs <=< peekVar)
  putVar var $
    DriverState
      { node: node'
      , vtree: vtree'
      , component: ds.component
      , state: ds.state
      , children
      , mkOrdBox: ds.mkOrdBox
      , selfRef: ds.selfRef
      , handler: ds.handler
      , keyId: ds.keyId
      , fresh: ds.fresh
      }

renderChild
  :: forall f g eff p
   . (f ~> Aff (HalogenEffects eff))
  -> Ref Int
  -> (p -> OrdBox p)
  -> Ref (M.Map (OrdBox p) (AVar (DriverStateX g eff)))
  -> Ref (M.Map (OrdBox p) (AVar (DriverStateX g eff)))
  -> Ref (LifecycleHandlers eff)
  -> ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)
  -> Aff (HalogenEffects eff) V.VTree
renderChild handler fresh mkOrdBox childrenInRef childrenOutRef lchs =
  unComponentSlot \p ctor k -> do
    childrenIn <- liftEff $ readRef childrenInRef
    var <- case M.pop (mkOrdBox p) childrenIn of
      Just (Tuple existing childrenIn') -> do
        liftEff $ writeRef childrenInRef childrenIn'
        pure existing
      Nothing ->
        runComponent (maybe (pure unit) handler <<< k) fresh lchs (force ctor)
    liftEff $ modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
    pure <<< unDriverStateX (\st -> V.widget st.keyId st.node) =<< peekVar var

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall eff a
   . Aff (HalogenEffects eff) a
  -> Eff (HalogenEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))

peekVar :: forall eff a. AVar a -> Aff (avar :: AVAR | eff) a
peekVar v = do
  a <- takeVar v
  putVar v a
  pure a
