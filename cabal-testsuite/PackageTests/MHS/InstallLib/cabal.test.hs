import Test.Cabal.Prelude

import System.Directory (listDirectory)

-- cabal install --lib with MicroHs, if it is installed: the library, with its
-- C source and its header, goes where MicroCabal installs packages, from where
-- mhs compiles a program that uses it, without cabal.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    -- MicroCabal installs into $CABALDIR if it is set.
    recordMode DoNotRecord $ withEnv [("CABALDIR", Nothing)] $ do
        cabal' "v2-install" ["--lib"] >>= assertOutputContains "Installed mhs-installlib-1.0"
        env <- getTestEnv
        -- The home directory of the test holds its .cabal directory.
        let userDir = takeDirectory (testCabalDir env) </> ".mcabal"
        [versionDir] <- liftIO (listDirectory userDir)
        let exe = testWorkDir env </> "prog"
        _ <- runProgramM mhsProgram ["-a" ++ userDir </> versionDir, "prog/Prog.hs", "-o" ++ exe] Nothing
        runM exe [] Nothing >>= assertOutputContains "triple 14 = 42"
