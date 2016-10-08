{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process
-- import Text.InterpolatedString.Perl6 (q)

-- TODO: Make configurable via .haskriptrc
stackExe :: String
stackExe = "stack"

data Args = Args
  { scriptPath :: FilePath
  }

main :: IO ()
main = do
  Args{..} <- parseArgs
  ensureHaskriptWorkDir
  let (_, scriptRelPath) = splitDrive scriptPath
      scriptTargetPath = haskriptWorkDir </> scriptRelPath
  skipCompile <- contentsAreIdentical scriptPath scriptTargetPath
  unless skipCompile $ doCompile scriptPath scriptTargetPath
  runScript scriptTargetPath

-- TODO: Redirect handles (stdin, stdout, stderr) accordingly
runScript :: FilePath -> IO ()
runScript target = do
  (code, out, err) <- readProcessWithExitCode stackExe ["--stack-yaml", target </> "stack.yaml", "exec", "--", takeFileName target] ""
  putStrLn out
  hPutStrLn stderr err
  exitWith code

-- | This copies the file from 'src' to 'target'. Note that each file gets its
-- own project, so a 'src' of "/foo/bar.hs" will actually copy the script to
-- "~/.haskript-work/foo/bar.hs/bar.hs"
doCompile :: FilePath -> FilePath -> IO ()
doCompile src target = do
  let targetName = takeFileName target
  createDirectoryIfMissing True target
  -- Duplicate the file name in the path, see the haddock comment above.
  let fullTargetPath = target </> targetName
  copyFile src fullTargetPath
  -- TODO: Add args from -- stack comment.
  -- contents <- readFile fullTargetPath
  createSetupFile target
  createCabalFile target
  createStackFile target
  (code, _, err) <- readProcessWithExitCode stackExe ["build", "--stack-yaml", target </> "stack.yaml"] ""
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> hPutStrLn stderr err >> exitWith code

createSetupFile :: FilePath -> IO ()
createSetupFile target = writeFile (target </> "Setup.hs") $ unlines
  [ "import Distribution.Simple"
  , "main = defaultMain"
  ]

-- TODO: Add args from -- stack comment.
createCabalFile :: FilePath -> IO ()
createCabalFile target = writeFile (target </> baseName ++ ".cabal") $ unlines
  [ "name: " ++ baseName
  , "version: 0"
  , "cabal-version: >=1.10"
  , "executable " ++ scriptName
  , "  hs-source-dirs: ."
  , "  main-is: " ++ scriptName
  , "  build-depends: base"
  ]
  where
  scriptName = takeFileName target
  (baseName, _) = splitExtensions scriptName

-- TODO: Add args from -- stack comment.
createStackFile :: FilePath -> IO ()
createStackFile target = writeFile (target </> "stack.yaml") $ unlines
  [ "resolver: lts-7.2"
  , "packages:"
  , "- '.'"
  ]

usage :: String
usage = "Usage: haskript path/to/script"
-- usage = [q|
--
-- |]

parseArgs :: IO Args
parseArgs = getArgs >>= \case
  [scriptPath] -> return Args {..}
  _ -> hPutStrLn stderr usage >> exitFailure

homeDir :: FilePath
{-# NOINLINE homeDir #-}
homeDir = unsafePerformIO getHomeDirectory

haskriptWorkDir :: FilePath
haskriptWorkDir = homeDir </> ".haskript-work"

ensureHaskriptWorkDir :: IO ()
ensureHaskriptWorkDir = createDirectoryIfMissing True haskriptWorkDir

contentsAreIdentical :: FilePath -> FilePath -> IO Bool
contentsAreIdentical f1 f2 = do
  (e1, e2) <- (,) <$> doesFileExist f1 <*> doesFileExist f2
  if not $ e1 && e2 then return False else do
    c1 <- readFile f1
    c2 <- readFile f2
    return $ c1 == c2
