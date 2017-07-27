# purescript-react-space
## Combinator library to write React vDOM

[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)

It it is heavily inspired by [purescript-smolder](https://github.com/bodil/purescript-smolder).

# Example

Run
```sh
npm run example:build
npm run example:serve
```

```purescript
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
```
