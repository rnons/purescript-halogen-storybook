# purescript-halogen-storybook

<a href="https://pursuit.purescript.org/packages/purescript-halogen-storybook">
  <img src="https://pursuit.purescript.org/packages/purescript-halogen-storybook/badge"
       alt="purescript-halogen-storybook on Pursuit">
  </img>
</a>

A library to assemble examples or develop components separately.

[DEMO](https://rnons.github.io/purescript-halogen-storybook/)

## How to use

First define the stories. Each story consists of a name and a component. If the name is empty string, it will be rendered as the index page.

```purescript
import Foreign.Object as Object
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: forall m. Stories m
stories = Object.fromFoldable
  [ Tuple "" $ proxy ExpIndex.component -- override the index page
  , Tuple "count" $ proxy ExpCount.component
  , Tuple "input" $ proxy ExpInput.component
  ]
```

Then add a `runStorybook` line to your main function. That's it.

```purescript
main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Nothing  -- pass `Just HH.PlainHTML` to override the logo
      }
```

You need to include the CSS by yourself, use [Storybook.css](https://github.com/rnons/purescript-halogen-storybook/blob/master/examples/src/Storybook.css) as an example.
