module React.Spaces
  ( Space(..)
  , SpaceF(SpaceF)
  , SpaceM
  , cls
  , (^)
  , cls'

  , (^^)
  , dCls'
  , (^+)
  , text
  , empty
  , element
  , elements
  , children
  , rDOMNode

  , class Propertable, with
  , (!)
  
  , withClass
  , (!.)
  , withId
  , (!#)

  , render
  , renderIn
  ) where

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, execState, state)
import Data.Array as A
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, sequence_)
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, Unit, map, pure, unit, ($), (<<<), (<>))
import React (ReactClass, ReactElement, createElement, createElementDynamic)
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM (text) as R
import React.DOM.Props (Props, className, _id)

data Space a props
  = DomNode String (Array Props) IsDynamic SpaceM a
  | ReactClassNode (ReactClass props) props IsDynamic SpaceM a
  | ReactElementNode ReactElement a
  | ChildrenNode (Array ReactElement) a
  | Empty a

mapSpace :: forall a b props. (a -> b) -> Space a props -> Space b props
mapSpace f (ReactClassNode cls_ props dynamic children_ a) = ReactClassNode cls_ props dynamic children_ (f a)
mapSpace f (DomNode tag props dynamic children_ a) = DomNode tag props dynamic children_ (f a)
mapSpace f (ChildrenNode rs a) = ChildrenNode rs (f a)
mapSpace f (ReactElementNode el a) = ReactElementNode el (f a)
mapSpace f (Empty a) = Empty (f a)

-- | `SpaceF` functor generating Free monad
newtype SpaceF a = SpaceF (Exists (Space a))

derive instance newtypeSpaceF :: Newtype (SpaceF a) _

instance functorPursFF :: Functor SpaceF where
  map f = over SpaceF $ runExists (mkExists <<< mapSpace f)

-- | Free monad which is used to build react's vDOM tree.
type SpaceM = Free SpaceF Unit

-- | Class without children.
cls :: forall props. (ReactClass props) -> props -> SpaceM
cls c p = liftF $ SpaceF (mkExists (ReactClassNode c p (IsDynamic false) empty unit))

infixl 3 cls as ^

-- | Class with children.
cls' :: forall props. (ReactClass props) -> props -> SpaceM -> SpaceM
cls' c p r = liftF $ SpaceF (mkExists (ReactClassNode c p (IsDynamic false) r unit))

infixl 3 cls' as ^^

-- | Class with dynamic children.
dCls' :: forall props. (ReactClass props) -> props -> SpaceM -> SpaceM
dCls' c p r = liftF $ SpaceF (mkExists (ReactClassNode c p (IsDynamic false) r unit))

infixl 3 dCls' as ^+

-- | React vDOM node.  You'd rather want to use `React.Spaces.DOM` or `React.Spaces.DOM.Dynamic`.
rDOMNode :: String -> Array Props -> IsDynamic -> SpaceM -> SpaceM
rDOMNode tag props dyn r = liftF $ SpaceF (mkExists (DomNode tag props dyn r unit))

-- | Text node.
text :: String -> SpaceM
text s = liftF $ SpaceF $ mkExists $ ReactElementNode (R.text s) unit

-- | Empty node.
empty :: SpaceM
empty = liftF (SpaceF (mkExists (Empty unit)))

-- | Single element node.
element :: ReactElement -> SpaceM
element el = liftF $ SpaceF $ mkExists $ ReactElementNode el unit

-- | Element with children
element' :: (Array ReactElement -> ReactElement) -> SpaceM -> SpaceM
element' fn chldrn = element $ fn (render chldrn)

-- | Add multiple elements at once
-- |
-- | Children can be implement using `element` (as `sequence_ <<< map element`)
-- | but this will be more performant.
children :: Array ReactElement -> SpaceM
children rs = liftF (SpaceF (mkExists (ChildrenNode rs unit)))

-- | Alias for `children`.
elements :: forall f. Functor f => Foldable f => f ReactElement -> SpaceM
elements = sequence_ <<< map element

class Propertable a where
  -- | Add a property to a vDOM node.
  with :: a -> Props -> a

-- | Combinator which adds `React.DOM.Props.Props` properties to react vDOM nodes.
-- | ```purescript
-- |  div ! className "container" ! onClick clickHandler $ do text "Hello"
-- | ```
infixl 4 with as !

withClass :: forall a. Propertable a => a -> String -> a
withClass elem name = elem ! className name

-- | Combinator which adds `className` prop.
infixl 4 withClass as !.

withId :: forall a. Propertable a => a -> String -> a
withId elem name = elem ! _id name

-- | Combinator which adds `_id` prop.
infixl 4 withId as !#

withAttribute :: forall a. SpaceF a -> Props -> SpaceF a
withAttribute (SpaceF sp) p = SpaceF $ runExists (mkExists <<< withAttr p) sp

withAttr :: forall props a. Props -> Space a props -> Space a props
withAttr p (DomNode s ps dyn r a) = DomNode s (A.snoc ps p) dyn r a
withAttr _ r = r

instance propertableSpaceM :: Propertable (Free SpaceF Unit) where
  with f p = hoistFree (\a -> withAttribute a p) f

instance propertableSpaceF :: Propertable (Free SpaceF Unit -> Free SpaceF Unit) where
  with f p m = f m `with` p

renderItem :: forall a. SpaceF a -> State (Array ReactElement) a
renderItem (SpaceF e) = runExists renderItem' e
  where
    renderItem' :: forall props. Space a props -> State (Array ReactElement) a
    renderItem' (DomNode tag props dynamic chldrn rest)
      = state \s -> Tuple rest $ A.snoc s (mkDOM dynamic tag props (render chldrn))
    renderItem' (ReactClassNode cls_ props (IsDynamic d) chldrn rest)
      = let createElement_ = if d then createElementDynamic else createElement
        in state \s -> Tuple rest $ A.snoc s (createElement_ cls_ props (render chldrn))
    renderItem' (ReactElementNode el rest)
      = state \s -> Tuple rest (A.snoc s el)
    renderItem' (ChildrenNode rs rest)
      = state \s -> Tuple rest $ s <> rs
    renderItem' (Empty rest) = pure rest

-- | Render `SpaceM` momnad as an `Array ReactElement`.
render :: SpaceM -> Array ReactElement
render f = execState (foldFree renderItem f) []

-- | Render `SpaceM` monad inside a wrapper element.
-- | ``` purescript
-- | btn :: forall eff. ReactSpec Unit Unit eff
-- | btn = (spec unit render)
-- |  where
-- |    render this =
-- |      pure $ renderIn React.DOM.div' do
-- |        button ! className "btn" ! onClick (handleClick this) $ do
-- |          text "Click me!"
-- |
-- |    handleClick thie ev = pure unit
-- | ```
renderIn :: (Array ReactElement -> ReactElement) -> SpaceM -> ReactElement
renderIn cnt = cnt <<< render
