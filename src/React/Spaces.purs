module React.Spaces where

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, execState, state)
import Data.Array as A
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, over, wrap)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, Unit, id, pure, unit, ($), (<<<))
import React (ReactClass, ReactElement, createElement, createElementDynamic)
import React.DOM (IsDynamic(..), mkDOM, text)
import React.DOM.Props (Props)

class ReactProps n r | n -> r where
  wrapProps :: r -> n

instance reactPropsNewtype :: (Newtype n r) => ReactProps n r where
  wrapProps = wrap

instance reactPropsRecord :: ReactProps (Record r) (Record r) where
  wrapProps = id

data Space a props
  = ReactClassNode (ReactClass props) props IsDynamic SpaceM a
  | DomNode String (Array Props) IsDynamic SpaceM a
  | TextNode String a
  | Empty a

mapSpace :: forall a b props. (a -> b) -> Space a props -> Space b props
mapSpace f (ReactClassNode cls props dynamic children a) = ReactClassNode cls props dynamic children (f a)
mapSpace f (DomNode tag props dynamic children a) = DomNode tag props dynamic children (f a)
mapSpace f (TextNode s a) = TextNode s (f a)
mapSpace f (Empty a) = Empty (f a)

-- | `SpaceF` functor generating Free monad
newtype SpaceF a = SpaceF (Exists (Space a))

derive instance newtypeSpaceF :: Newtype (SpaceF a) _

instance functorPursFF :: Functor SpaceF where
  map f = over SpaceF $ runExists (mkExists <<< mapSpace f)

type SpaceM = Free SpaceF Unit

rClsNode :: forall props. (ReactClass props) -> props -> SpaceM -> SpaceM
rClsNode c p r = liftF $ SpaceF (mkExists (ReactClassNode c p (IsDynamic false) r unit))

rDOMNode :: String -> Array Props -> IsDynamic -> SpaceM -> SpaceM
rDOMNode tag props dyn r = liftF $ SpaceF (mkExists (DomNode tag props dyn r unit))

rTextNode :: String -> SpaceM
rTextNode s = liftF $ SpaceF $ mkExists $ TextNode s unit

rEmptyNode :: SpaceM
rEmptyNode = liftF (SpaceF (mkExists (Empty unit)))

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

instance poertableSpaceF :: Propertable (Free SpaceF Unit -> Free SpaceF Unit) where
  with f p m = f m `with` p

renderItem :: forall a. SpaceF a -> State (Array ReactElement) a
renderItem (SpaceF e) = runExists renderItem' e
  where
    renderItem' :: forall props. Space a props -> State (Array ReactElement) a
    renderItem' (ReactClassNode cls props (IsDynamic d) children rest)
      = let cld = render children
            createElement_ = if d then createElementDynamic else createElement
        in state \s -> Tuple rest $ A.snoc s (createElement_ cls props cld)
    renderItem' (DomNode tag props dynamic children rest)
      = let cld = render children
        in state \s -> Tuple rest $ A.snoc s (mkDOM dynamic tag props cld)
    renderItem' (TextNode str rest)
      = state \s -> Tuple rest $ A.snoc s (text str)
    renderItem' (Empty rest) = pure rest

render :: SpaceM -> Array ReactElement
render f = execState (foldFree renderItem f) []

renderIn :: (Array ReactElement -> ReactElement) -> SpaceM -> ReactElement
renderIn cnt = cnt <<< render
