import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
import Data.List (foldl', isInfixOf)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

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
      env <- getTestEnv
      cabalPath <- programPathM cabalProgram
      baseEnv <- liftIO getEnvironment
      let mergedEnv =
            foldl'
              (\acc (k, mv) -> maybe (filter ((/= k) . fst) acc) (\v -> (k, v) : filter ((/= k) . fst) acc) mv)
              baseEnv
              (testEnvironment env)
      (exitCode, out, err) <-
        liftIO $
          readCreateProcessWithExitCode
            ( (proc cabalPath ["v2-run", "foo"])
                { env = Just mergedEnv
                , cwd = Just (testCurrentDir env)
                }
            )
            ""
      assertEqual "v2-run should succeed" ExitSuccess exitCode
      assertBool "expected executable output on stdout" ("Hello World" `isInfixOf` out)
      assertBool
        "cabal status output should not be sent to stdout"
        (all (not . (`isInfixOf` out)) ["Resolving dependencies...", "Build profile:", "In order, the following will be built:", "Up to date"])
      assertBool
        "expected cabal status output on stderr"
        (any (`isInfixOf` err) ["Resolving dependencies...", "Build profile:", "In order, the following will be built:", "Up to date"])
