import Test.Cabal.Prelude
import Control.Monad ( (>=>) )

main = cabalTest $ do
    -- Some different ways of calling an executable that should all work
    -- on a single-exe single-package project
    mapM_ (cabal' "v2-run" >=> assertOutputContains "Hello World")
         [ ["foo"]
         , ["Single"]
         , []
         , ["Single:foo"]
         , ["exe:foo"] ]
    -- non-existent exe
    fails (cabal' "v2-run" ["bar"]) >>= assertOutputDoesNotContain "Hello World"
    recordMode DoNotRecord $ do
      cabal "v2-clean" []
      output <- cabal' "v2-run" ["foo"]
      assertOutputContains "Hello World" output
      assertOutputContains "Build profile:" output
      assertOutputContains "In order, the following will be built:" output
      assertOutputContains "Preprocessing executable" output
