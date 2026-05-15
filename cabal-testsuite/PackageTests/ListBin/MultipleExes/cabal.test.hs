import System.FilePath ( joinPath )

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/9714
-- When no target is given and there are multiple executables,
-- list-bin should print the path to all of them.

main = cabalTest . void $ do
    res <- cabal' "list-bin" []

    let path1 = joinPath ["MultipleExes-1.0.0", "build", "exe1", "exe1"]
    let path2 = joinPath ["MultipleExes-1.0.0", "build", "exe2", "exe2"]
    assertOutputContains path1 res
    assertOutputContains path2 res
