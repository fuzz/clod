import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import Control.Monad

-- | Custom setup to generate and install man pages
main = defaultMainWithHooks simpleUserHooks
  { postBuild = \args buildFlags pkg lbi -> do
      -- Run the standard postBuild first
      postBuild simpleUserHooks args buildFlags pkg lbi
      -- Generate man pages after build
      generateManPages lbi
  }

-- | Generate man pages using the generate-man-pages.sh script
generateManPages :: LocalBuildInfo -> IO ()
generateManPages lbi = do
  -- Path to generate-man-pages.sh script
  let scriptPath = "bin" </> "generate-man-pages.sh"
      -- Directory where man pages will be generated
      -- Generate into the builddir/man directory which is referenced in data-files
      manOutDir = buildDir lbi
      -- Output argument for the script
      outputArg = manOutDir
  
  -- Check if pandoc is available
  pandocExists <- doesPandocExist
  
  -- Only generate man pages if pandoc is available
  when pandocExists $ do
    putStrLn "Generating man pages..."
    -- Create man output directory structure
    createDirectoryIfMissing True (manOutDir </> "man" </> "man1")
    createDirectoryIfMissing True (manOutDir </> "man" </> "man7")
    createDirectoryIfMissing True (manOutDir </> "man" </> "man8")
    
    -- Run the generate-man-pages.sh script
    exitCode <- system $ scriptPath ++ " " ++ (manOutDir </> "man")
    case exitCode of
      ExitSuccess -> putStrLn "Man pages generated successfully"
      ExitFailure code -> putStrLn $ "Man page generation failed with code " ++ show code

-- | Check if pandoc is available on the system
doesPandocExist :: IO Bool
doesPandocExist = do
  (_, _, _, ph) <- createProcess (shell "which pandoc")
                    { std_out = CreatePipe, std_err = CreatePipe }
  exit <- waitForProcess ph
  return $ exit == ExitSuccess