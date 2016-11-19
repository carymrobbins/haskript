{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Monoid
import Data.Time
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

-- TODO: Make configurable via .haskriptrc
stackExe :: String
stackExe = "stack"

data Args = Args
  { scriptPath :: FilePath
  , scriptArgs :: [String]
  } deriving Show

data Project = Project
  { projectName :: String
  , projectPath :: String
  , projectModifiedTime :: UTCTime
  , projectFiles :: [ProjectFile]
  } deriving Show

data ProjectFile = ProjectFile
    { projectFileName :: String
    , projectFileContent :: String
    } deriving Show

projectFileAbsPath :: Project -> ProjectFile -> String
projectFileAbsPath Project{..} ProjectFile{..} = projectPath </> projectFileName

usage :: String
usage = "Usage: haskript path/to/script"

main :: IO ()
main = do
  args@Args{..} <- parseArgs
  project <- createProjectData args
  shouldWrite <- requiresWriteProject project
  when shouldWrite $ do
    writeProject project
    buildProject project
  exe <- findCompiledExe project
  putStrLn $ "Found executable: " <> exe
  -- TODO: rawSystem doesn't seem to work
  exitWith =<< rawSystem exe scriptArgs

errExit :: String -> IO a
errExit msg = hPutStrLn stderr msg >> exitFailure

whenM :: Monad m => m Bool -> m () -> m ()
whenM predM onTrue = do
  p <- predM
  when p onTrue

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM predM onFalse = do
  p <- predM
  unless p onFalse

allM :: Monad m => (m a -> m Bool) -> [m a] -> m Bool
allM _ [] = return True
allM f (x:xs) = do
  b <- f x
  if b then allM f xs else return False

anyM :: Monad m => (m a -> m Bool) -> [m a] -> m Bool
anyM _ [] = return False
anyM f (x:xs) = do
  b <- f x
  if b then return True else anyM f xs

parseArgs :: IO Args
parseArgs = getArgs >>= \case
  ["--help"] -> putStrLn usage >> exitSuccess

  scriptPath:scriptArgs -> do
    unlessM (doesFileExist scriptPath) $ errExit $ "File does not exist: " <> scriptPath
    return Args {..}

  _ -> errExit usage

findCompiledExe :: Project -> IO String
findCompiledExe Project{..} = do
  -- TODO: Handle Windows
  (exitCode, out, err) <- readProcessWithExitCode stackExe
      [ "--stack-yaml", projectPath </> "stack.yaml",
        "exec", "--",
        "command", "-v", projectName
      ] ""
  case exitCode of
    ExitSuccess -> return out
    ExitFailure _ -> do
      hPutStrLn stderr $ "Could not locate compiled executable for " <> projectName
      hPutStrLn stderr err
      exitWith exitCode

buildProject :: Project -> IO ()
buildProject Project{..} = do
  (exitCode, _, err) <- readProcessWithExitCode stackExe
      ["--stack-yaml", projectPath </> "stack.yaml", "build"] ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      hPutStrLn stderr err
      exitWith exitCode

requiresWriteProject :: Project -> IO Bool
requiresWriteProject Project{..} = anyM id actions
  where
  actions =
    [ not <$> doesFileExist projectPath
    , anyM id $ map requiresWriteProjectFile projectFiles
    ]
  requiresWriteProjectFile :: ProjectFile -> IO Bool
  requiresWriteProjectFile ProjectFile{..} = do
    let absPath = projectPath </> projectFileName
    exists <- doesFileExist absPath
    modTime <- getModificationTime absPath
    return $ not exists || projectModifiedTime > modTime

writeProject :: Project -> IO ()
writeProject Project{..} = do
  createDirectoryIfMissing True projectPath
  forM_ projectFiles $ \ProjectFile{..} ->
    writeFile (projectPath </> projectFileName) projectFileContent

createProjectData :: Args -> IO Project
createProjectData args@Args{..} = do
  p <- emptyProject
  let cabalFile = buildCabalFile p
  mainFile <- buildMainFile args
  return $ p { projectFiles = [cabalFile, mainFile, setupFile, stackFile] }
  where
  emptyProject = do
    let projectFiles = []
    let projectName = takeBaseName scriptPath
    fullScriptPath <- makeAbsolute scriptPath
    let projectPath = haskriptWorkDir </> (snd . splitDrive $ fullScriptPath)
    projectModifiedTime <- getModificationTime scriptPath
    return Project{..}

mainFileName :: String
mainFileName = "Main.hs"

buildMainFile :: Args -> IO ProjectFile
buildMainFile Args{..} = do
  let projectFileName = mainFileName
  projectFileContent <- readFile scriptPath
  return ProjectFile{..}

buildCabalFile :: Project -> ProjectFile
buildCabalFile Project{..} = ProjectFile{..}
  where
  projectFileName = projectName <> ".cabal"
  projectFileContent = unlines
    [ "name: " <> projectName
    , "version: 0"
    , "cabal-version: >=1.10"
    , "build-type: Simple"
    , ""
    , "executable " <> projectName
    , "  hs-source-dirs: ."
    , "  main-is: " <> mainFileName
    -- TODO: Add args from -- stack comment.
    , "  build-depends: base"
    , "  default-language: Haskell2010"
    ]

setupFile :: ProjectFile
setupFile = ProjectFile{..}
  where
  projectFileName = "Setup.hs"
  projectFileContent = unlines
    [ "import Distribution.Simple"
    , "main = defaultMain"
    ]


stackFile :: ProjectFile
stackFile = ProjectFile{..}
  where
  projectFileName = "stack.yaml"
  projectFileContent = unlines
    [ "resolver: lts-7.2"
    , "packages:"
    , "  - '.'"
    ]

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
