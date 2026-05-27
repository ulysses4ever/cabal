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
      configuredCabal <- requireProgramM cabalProgram
      baseEnv <- liftIO getEnvironment
      let mergedEnv =
            foldl'
              (\acc (k, mv) -> maybe (filter ((/= k) . fst) acc) (\v -> (k, v) : filter ((/= k) . fst) acc) mv)
              baseEnv
              (testEnvironment env)
          streamCheckBuildDir = testDistDir env </> "stream-check"
          verbosityFlags = "-vverbose +markoutput +nowrap"
          statusMessages =
            [ "Build profile:"
            , "In order, the following will be built:"
            ]
      (exitCode, out, err) <-
        liftIO $
          readCreateProcessWithExitCode
            ( (proc (programPath configuredCabal) ["v2-run", verbosityFlags, "--builddir", streamCheckBuildDir, "-j1", "foo"])
                { cwd = Just (testCurrentDir env)
                , env = Just mergedEnv
                }
            )
            ""
      assertEqual "v2-run should succeed" ExitSuccess exitCode
      assertBool "expected executable output on stdout" ("Hello World" `isInfixOf` out)
      assertBool
        "cabal status output should not be sent to stdout"
        (all (not . (`isInfixOf` out)) statusMessages)
      assertBool
        "expected cabal status output on stderr"
        (any (`isInfixOf` err) statusMessages)
      assertBool
        "expected preprocessing status output on stderr"
        ("Preprocessing executable" `isInfixOf` err)
