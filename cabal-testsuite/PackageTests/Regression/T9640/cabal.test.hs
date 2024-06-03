import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  skipUnlessGhcVersion ">= 9.10"
  cabal "build" ["depend-on-custom-with-exe"]
