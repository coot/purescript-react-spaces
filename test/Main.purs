module Test.Main where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, map, pure, show, unit, void, ($), (+), (<$>), (<<<), (>>=))
import React (ReactClass, ReactProps, ReactRefs, ReactState, ReadOnly, ReadWrite, createClass, createClassStateless, createElement, readState, spec, transformState)
import React.DOM as D
import React.DOM.Props as P
import React.Space (rClsNode, rDomNode, rEmptyNode, rTextNode, renderIn)
import ReactDOM (render)
import ReactHocs (setDisplayName)
import Unsafe.Coerce (unsafeCoerce)

newtype Greeting eff = Greeting
  { greeting :: String
  , onChange :: String -> Eff eff Unit }

gCls :: forall eff. ReactClass (Greeting (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff))
gCls = setDisplayName "Greeting" $ createClassStateless (\(Greeting { greeting, onChange }) -> renderIn D.div' do
  rDomNode "div" [] do
    rDomNode "label" [] do
      rDomNode "div" [] do
        rTextNode "Greeting"
      rDomNode "input" [P.value greeting, P.onChange (handleChange onChange) ] rEmptyNode)

  where 
    handleChange onChange ev = do
      v <- pure (unsafeCoerce ev).target.value
      onChange v

newtype Counter eff = Counter
  { counter :: Int
  , onClick :: Eff eff Unit
  }
cCls :: forall eff. ReactClass (Counter (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff))
cCls = setDisplayName "Counter" $ createClassStateless (\(Counter { counter, onClick }) -> renderIn D.div' do
  rDomNode "div" [] do
    rDomNode "span" [] do
      rTextNode (show counter)
    rDomNode "button" [P.onClick (handleClick onClick)] do
      rTextNode "count")

  where
    handleClick onClick ev = onClick

base :: ReactClass Unit
base = createClass (spc { displayName = "BaseClass" })
  where
    spc = spec { greeting: "Hello world!", counter: 0 } (map (renderIn D.div') <$> renderFn)

    handleGreeting this greeting = do
      transformState this (_ { greeting = greeting })

    handleCounter this = do
      transformState this (\st@{counter} -> st { counter = (counter + 1) })

    renderFn this = do
      { counter, greeting } <- readState this
      pure $ do
        rClsNode gCls (Greeting { greeting, onChange: handleGreeting this }) rEmptyNode
        rClsNode cCls (Counter { counter, onClick: handleCounter this }) rEmptyNode

findElmById :: forall e. ElementId -> Eff (dom :: DOM | e) Element
findElmById _id = do
  el <- window >>= document >>= getElementById _id <<< castDocument
  pure $ unsafePartial fromJust el

 where
    castDocument = documentToNonElementParentNode <<< htmlDocumentToDocument

main :: forall e. Eff (dom :: DOM | e) Unit
main = do
  el <- findElmById (ElementId "app")
  void $ render (createElement base unit []) el
