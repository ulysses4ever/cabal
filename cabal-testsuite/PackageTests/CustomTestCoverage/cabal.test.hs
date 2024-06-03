import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnlessGhcVersion ">= 9.10"
    cabal "test" ["--enable-coverage"]
