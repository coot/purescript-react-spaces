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
import Prelude (Unit, bind, discard, map, not, pure, show, unit, void, ($), (+), (<<<), (>>=))
import React (ReactClass, ReactProps, ReactRefs, ReactState, ReadOnly, ReadWrite, createClass, createClassStateless', createElement, readState, spec, transformState)
import React.DOM as D
import React.DOM.Props (className, onClick)
import React.DOM.Props as P
import React.Spaces (children, cls, empty, renderIn, text, (!))
import React.Spaces.DOM (button, div, h1, input, label, span)
import ReactDOM (render)
import ReactHocs (setDisplayName)
import Unsafe.Coerce (unsafeCoerce)

newtype Greeting eff = Greeting
  { name :: String
  , onChange :: String -> Eff eff Unit }

greeting :: forall eff. ReactClass (Greeting (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff))
greeting = setDisplayName "Greeting" $ createClassStateless' \(Greeting { name, onChange }) chldrn
  -> renderIn D.div' do
    div ! className "greeting" $ do
      label do
        div do
          h1 do
            children chldrn
            text " "
            text name
            text if not (S.null name) then "!" else ""
        input ! P.value name ! P.onChange (handleChange onChange) $ empty

  where 
    handleChange onChange ev = do
      v <- pure (unsafeCoerce ev).target.value
      onChange v

newtype Counter eff = Counter
  { counter :: Int
  , onClick :: Eff eff Unit
  }
counter :: forall eff. ReactClass (Counter (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff))
counter = setDisplayName "Counter" $ createClassStateless' \(Counter { counter: c, onClick: onClick' }) chldrn
  -> renderIn D.div' do
    div do
      children chldrn
      span $ text (show c)
      button ! onClick (handleClick onClick') $ do
        text "count"

  where
    handleClick onClick ev = onClick

base :: ReactClass Unit
base = createClass (spc { displayName = "BaseClass" })
  where
    spc = spec { name: "Saylor", counter: 0 } (map (renderIn D.div') <<< renderFn)

    handleName this name = do
      transformState this (_ { name = name })

    handleCounter this = do
      transformState this (\st@{counter: c} -> st { counter = c + 1 })

    renderFn this = do
      { counter: c, name } <- readState this
      pure $ do
        cls greeting (Greeting { name, onChange: handleName this }) $
          do span $ text "Hello"

        cls counter (Counter { counter: c, onClick: handleCounter this }) $
          do h1 $ text "Count days on the sea..."

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
