## tasty-focus

Simple focus mechanism for `tasty`, similar to `hspec`.

Mark the root of your test tree with `withFocus`.
Then, if any of the subtrees of your test suite are marked with `focus`, only those test trees will be run.


### Example
In this example, only `barTests` will run. Removing either `focus` or `withFocus` will run the entire tree again.

```haskell
main :: IO ()
main = defaultMain . withFocus $
  testGroup "tests"
    [ fooTests
    , testGroup "subgroup"
      [ focus barTests
      , bazTests
	  ]
	, quuxTests
    ]
```
