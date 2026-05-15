import System.FilePath ( joinPath )

import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/9714
-- When no target is given and there is exactly one executable,
-- list-bin should print the path to that executable.

main = cabalTest $ do
    -- Use DoNotRecord to avoid maintaining a golden file with build output
    res <- recordMode DoNotRecord $ cabal' "list-bin" []

    -- The binary is somewhere under the build directory for myexe
    assertOutputContains (joinPath ["myexe", "build", "myexe", "myexe"]) res
