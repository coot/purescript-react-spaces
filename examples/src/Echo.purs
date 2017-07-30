module Examples.Echo where

import Data.Array as A
import Data.Foldable (minimum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Traversable (sequence_)
import Prelude (Unit, bind, discard, map, pure, show, ($), (-), (/), (<$>), (<>), (==), (||))
import React (ReactClass, ReactSpec, createClass, createClassStateless, readState, spec, transformState)
import React.DOM as React.DOM
import React.DOM.Props (className, onChange, style, value)
import React.Spaces (SpaceM, renderIn, text, (!), (^))
import React.Spaces.DOM (code, div, h2, h3, input, label, small, span)
import Unsafe.Coerce (unsafeCoerce)

dedent :: String -> String
dedent s = fromMaybe s $ do
    c <- common <$> tl
    lns <- map (S.drop c) <$> tl
    pure $ S.joinWith "\n" lns
  where
    tl :: Maybe (Array String)
    tl = A.tail $ S.split (S.Pattern "\n") s

    count :: String -> Int
    count s' = S.length $ S.takeWhile (\c -> c == ' ' || c == '\t') s'

    common :: Array String -> Int
    common lns = fromMaybe 0 $ minimum (count <$> lns)

echoCls :: ReactClass { echo :: String }
echoCls = createClassStateless \{ echo } ->
  let len = S.length echo
  in renderIn React.DOM.div' do
    h3 ! className "title" $ do
      sequence_
        $ A.mapWithIndex (\idx node -> node ! style { fontSize: show (1.0 - (toNumber idx) / (toNumber len)) <> "em" })
        $ A.replicate len (span $ text "echo ")
    sequence_
      -- add style to every top level echo node
      $ A.mapWithIndex (\idx node -> node ! style { fontSize: show (2.0 - (toNumber idx) / (toNumber len)) <> "em" })
      -- echo
      $ A.replicate len (div ! className "echo" $ text echo)

mSpec :: forall eff. ReactSpec Unit { mEcho :: Maybe String } eff
mSpec = spec { mEcho: Nothing } renderFn
  where
    handleChange this ev = do
      let v = (unsafeCoerce ev).target.value
      transformState this (_ { mEcho = if S.null v then Nothing else Just v })

    renderFn this = do
      { mEcho } <- readState this
      let mEchoNode = (echoCls ^ _) <$> { echo: _ } <$> mEcho
      pure $ renderIn React.DOM.div' $ do
	h2 ! className "app" $ do
	  text "Echo App"
        div $ do
          small $ text "using `sequence_` to mount a conditional component"
          code ! style { display: "block", whiteSpace: "pre-wrap", padding: "10px 20px", backgroundColor: "#eee" } $ text $ dedent
            """
            label do
              div $
                text "echo"
              input ! value (fromMaybe "" mEcho) ! onChange (handleChange this)
            sequence_ (mEchoNode :: Maybe SpaceM)
            """
        label ! style { display: "block", margin: "20px 0px" } $ do
          div $
            text "echo"
          input ! value (fromMaybe "" mEcho) ! onChange (handleChange this)
        sequence_ (mEchoNode :: Maybe SpaceM)

echoApp :: ReactClass Unit
echoApp = createClass mSpec
