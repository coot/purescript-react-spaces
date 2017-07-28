# purescript-react-spaces
**Combinator library for writting React vDOM**

[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)

It it is heavily inspired by [purescript-smolder](https://github.com/bodil/purescript-smolder).

# Usage
The top level module exports

```purescript
renderIn :: (Array ReactElement -> ReactElement) -> SpaceM -> ReactElement
render :: SpaceM -> Array ReactElement
```

which you can chain with `render` method of your spec:
```purescript
s :: ReactSpec Unit Unit
s = spec unit (map (renderIn React.DOM.div') <$> render)
  where
    render :: ReactThis Unit Unit -> SpaceM
    render this = pure $
      div ! className "greeting" $ do
	text "Hello World!

```

# Example

Run
```sh
npm run example:build
npm run example:serve
```


It will compile the following simple example:
```purescript
greeting
  :: forall eff
   . ReactClass
      (Greeting
	( props :: ReactProps
	, refs :: ReactRefs ReadOnly
	, state :: ReactState ReadWrite
	| eff))
greeting = createClassStateless' (\(Greeting { name, onChange }) chldrn -> renderIn React.DOM.div' do
  div ! className "greeting" $ do
    label do
      div do
        h1 do
          children chldrn
          text " "
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
counter
  :: forall eff
   . ReactClass
      (Counter
	( props :: ReactProps
	, refs :: ReactRefs ReadOnly
	, state :: ReactState ReadWrite
	| eff))
counter = createClassStateless' (\(Counter { counter: c, onClick: onClick' }) chldrn -> renderIn React.DOM.div' do
  div do
    children chldrn
    span $ text (show c)
    button ! onClick (handleClick onClick') $ do
      text "count")

  where
    handleClick onClick ev = onClick

base :: ReactClass Unit
base = createClass (spc { displayName = "BaseClass" })
  where
    spc = spec { name: "John", counter: 0 } (map (renderIn React.DOM.div') <$> renderFn)

    handleName this name = do
      transformState this (_ { name = name })

    handleCounter this = do
      transformState this (\st@{counter} -> st { counter = (counter + 1) })

    renderFn this = do
      { counter, name } <- readState this
      pure $ do
	cls greeting (Greeting { name, onChange: handleName this }) $
	  do span $ text "Hello"
        cls counter (Counter { counter, onClick: handleCounter this }) $
	  do h1 $ "Count on the sea..."
```
