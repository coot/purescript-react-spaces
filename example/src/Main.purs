module Example.Main where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Data.String as S
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, map, not, pure, show, unit, void, ($), (+), (<$>), (<<<), (>>=))
import React (ReactClass, ReactProps, ReactRefs, ReactState, ReadOnly, ReadWrite, createClass, createClassStateless, createElement, readState, spec, transformState)
import React.DOM as D
import React.DOM.Props (className, onClick)
import React.DOM.Props as P
import React.Spaces (renderIn, (!))
import React.Spaces.DOM (button, cls, div, empty, input, label, span, text)
import ReactDOM (render)
import ReactHocs (setDisplayName)
import Unsafe.Coerce (unsafeCoerce)

newtype Greeting eff = Greeting
  { name :: String
  , onChange :: String -> Eff eff Unit }

gCls :: forall eff. ReactClass (Greeting (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff))
gCls = setDisplayName "Greeting" $ createClassStateless (\(Greeting { name, onChange }) -> renderIn D.div' do
  div ! className "greeting" $ do
    label do
      div do
        text "Hello "
        text name
        text if not (S.null name) then "!" else ""
      input ! P.value name ! P.onChange (handleChange onChange) $ empty
    )

  where 
    handleChange onChange ev = do
      v <- pure (unsafeCoerce ev).target.value
      onChange v

newtype Counter eff = Counter
  { counter :: Int
  , onClick :: Eff eff Unit
  }
cCls :: forall eff. ReactClass (Counter (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff))
cCls = setDisplayName "Counter" $ createClassStateless (\(Counter { counter, onClick: onClick' }) -> renderIn D.div' do
  div do
    span $ text (show counter)
    button ! onClick (handleClick onClick') $ do
      text "count")

  where
    handleClick onClick ev = onClick

base :: ReactClass Unit
base = createClass (spc { displayName = "BaseClass" })
  where
    spc = spec { name: "John", counter: 0 } (map (renderIn D.div') <$> renderFn)

    handleName this name = do
      transformState this (_ { name = name })

    handleCounter this = do
      transformState this (\st@{counter} -> st { counter = (counter + 1) })

    renderFn this = do
      { counter, name } <- readState this
      pure $ do
        cls gCls (Greeting { name, onChange: handleName this }) empty
        cls cCls (Counter { counter, onClick: handleCounter this }) empty

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
