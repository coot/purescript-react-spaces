module React.Spaces where

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, execState, state)
import Data.Array as A
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, Unit, pure, unit, ($), (<<<), (<>))
import React (ReactClass, ReactElement, createElement, createElementDynamic)
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM (text) as R
import React.DOM.Props (Props)

data Space a props
  = ReactClassNode (ReactClass props) props IsDynamic SpaceM a
  | DomNode String (Array Props) IsDynamic SpaceM a
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

type SpaceM = Free SpaceF Unit

cls :: forall props. (ReactClass props) -> props -> SpaceM -> SpaceM
cls c p r = liftF $ SpaceF (mkExists (ReactClassNode c p (IsDynamic false) r unit))

rDOMNode :: String -> Array Props -> IsDynamic -> SpaceM -> SpaceM
rDOMNode tag props dyn r = liftF $ SpaceF (mkExists (DomNode tag props dyn r unit))

text :: String -> SpaceM
text s = liftF $ SpaceF $ mkExists $ ReactElementNode (R.text s) unit

empty :: SpaceM
empty = liftF (SpaceF (mkExists (Empty unit)))

element :: ReactElement -> SpaceM
element el = liftF $ SpaceF $ mkExists $ ReactElementNode el unit

children :: Array ReactElement -> SpaceM
children rs = liftF (SpaceF (mkExists (ChildrenNode rs unit)))

class Propertable a where
  -- | Add a property to a vDOM node.
  with :: a -> Props -> a

infixl 4 with as !

withAttribute :: forall a. SpaceF a -> Props -> SpaceF a
withAttribute (SpaceF sp) p = SpaceF $ runExists (mkExists <<< withAttr) sp
  where
    withAttr :: forall props. Space a props -> Space a props
    withAttr (DomNode s ps dyn r a) = DomNode s (A.snoc ps p) dyn r a
    withAttr r = r

instance propertableSpaceM :: Propertable (Free SpaceF Unit) where
  with f p = hoistFree (\a -> withAttribute a p) f

instance propertableSpaceF :: Propertable (Free SpaceF Unit -> Free SpaceF Unit) where
  with f p m = f m `with` p

renderItem :: forall a. SpaceF a -> State (Array ReactElement) a
renderItem (SpaceF e) = runExists renderItem' e
  where
    renderItem' :: forall props. Space a props -> State (Array ReactElement) a
    renderItem' (ReactClassNode cls_ props (IsDynamic d) chldrn rest)
      = let createElement_ = if d then createElementDynamic else createElement
        in state \s -> Tuple rest $ A.snoc s (createElement_ cls_ props (render chldrn))
    renderItem' (DomNode tag props dynamic chldrn rest)
      = state \s -> Tuple rest $ A.snoc s (mkDOM dynamic tag props (render chldrn))
    renderItem' (ReactElementNode el rest)
      = state \s -> Tuple rest (A.snoc s el)
    renderItem' (ChildrenNode rs rest)
      = state \s -> Tuple rest $ s <> rs
    renderItem' (Empty rest) = pure rest

render :: SpaceM -> Array ReactElement
render f = execState (foldFree renderItem f) []

renderIn :: (Array ReactElement -> ReactElement) -> SpaceM -> ReactElement
renderIn cnt = cnt <<< render
