module React.Space where

import Control.Monad.Free (Free, foldFree, liftF)
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

-- I need GADT or type families to do this polymorphically over props
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

newtype SpaceF a = SpaceF (Exists (Space a))

derive instance newtypeSpaceF :: Newtype (SpaceF a) _

instance functorPursFF :: Functor SpaceF where
  map f = over SpaceF $ runExists (mkExists <<< mapSpace f)

type SpaceM = Free SpaceF Unit

rClsNode :: forall props. (ReactClass props) -> props -> SpaceM -> SpaceM
rClsNode c p r = liftF $ SpaceF (mkExists (ReactClassNode c p (IsDynamic false) r unit))

rDomNode :: String -> Array Props -> SpaceM -> SpaceM
rDomNode tag props r = liftF $ SpaceF (mkExists (DomNode tag props (IsDynamic false) r unit))

rTextNode :: String -> SpaceM
rTextNode s = liftF $ SpaceF $ mkExists $ TextNode s unit

rEmptyNode :: SpaceM
rEmptyNode = liftF (SpaceF (mkExists (Empty unit)))

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
