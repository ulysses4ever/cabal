import System.FilePath ( joinPath )

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/9714
-- When no target is given and there are multiple executables,
-- list-bin should print the path to all of them.

main = cabalTest $ do
    -- Use DoNotRecord to avoid maintaining a golden file with build output
    res <- recordMode DoNotRecord $ cabal' "list-bin" []

    -- Both binaries should be in the output
    assertOutputContains (joinPath ["exe1", "build", "exe1", "exe1"]) res
    assertOutputContains (joinPath ["exe2", "build", "exe2", "exe2"]) res
