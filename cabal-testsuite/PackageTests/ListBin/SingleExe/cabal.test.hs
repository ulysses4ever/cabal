import System.FilePath ( joinPath )

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/9714
-- When no target is given and there is exactly one executable,
-- list-bin should print the path to that executable.

main = cabalTest . void $ do
    res <- cabal' "list-bin" []

    let path = joinPath ["SingleExe-1.0.0", "build", "myexe", "myexe"]
    assertOutputContains path res
