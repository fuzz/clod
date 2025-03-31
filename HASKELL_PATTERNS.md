# Haskell Patterns and Best Practices

This document contains common Haskell patterns and best practices for efficient and idiomatic Haskell development. These patterns are particularly useful for human-AI collaboration, where clear communication and code understanding are essential.

## Functional Programming Patterns

### Pure Functions and Side Effects

- **Pure Functions**: Prefer pure functions over impure operations whenever possible. Pure functions are easier to reason about, test, and compose.
  ```haskell
  -- Pure function
  calculateTotal :: [Item] -> Price
  calculateTotal items = sum (map itemPrice items)
  
  -- Instead of this impure approach
  calculateTotal :: [Item] -> IO Price
  calculateTotal items = do
    forM items $ \item -> do
      logItemProcess item  -- Side effect!
      return (itemPrice item)
    ...
  ```

- **Effect Localization**: When side effects are necessary, localize them to the boundaries of your application. Keep your core logic pure.
  ```haskell
  -- Good: Centralized effects at the edge
  main :: IO ()
  main = do
    input <- readInput        -- IO at the boundary
    let result = process input -- Pure core logic
    writeOutput result        -- IO at the boundary
  ```

- **Resource Management**: Use higher-order functions like `bracket`, `withFile`, or `ResourceT` to ensure resources are properly acquired and released.
  ```haskell
  -- Ensures file is closed even if an exception occurs
  withConfigFile :: FilePath -> (Handle -> IO a) -> IO a
  withConfigFile path action = bracket 
    (openFile path ReadMode)  -- acquire
    hClose                    -- release
    action                    -- use
  ```

- **Error Handling**: Use types to represent errors rather than exceptions. Wrap impure code with `try`/`catch` and convert exceptions to domain-specific error types.
  ```haskell
  -- Domain-specific error type
  data AppError = FileNotFound FilePath | ParseError String | NetworkError
  
  -- Convert IO exceptions to domain errors
  readConfig :: FilePath -> IO (Either AppError Config)
  readConfig path = do
    result <- try (readFile path)
    case result of
      Left e -> return $ Left $ FileNotFound path
      Right content -> 
        case parseConfig content of
          Nothing -> return $ Left $ ParseError "Invalid config"
          Just config -> return $ Right config
  ```

### Type-Driven Development

- **Newtype Wrappers**: Use `newtype` to create distinct types for values that might otherwise be confused.
  ```haskell
  -- Without newtypes
  processUser :: String -> Int -> String -> IO ()  -- What do these mean?
  
  -- With newtypes
  newtype UserId = UserId String
  newtype Age = Age Int
  newtype Email = Email String
  
  processUser :: UserId -> Age -> Email -> IO ()  -- Much clearer!
  ```
  
- **Smart Constructors**: Use smart constructors to enforce invariants and hide implementation details.
  ```haskell
  module Email (Email, mkEmail, emailToText) where
  
  newtype Email = Email { _unEmail :: Text } -- Private constructor
  
  -- Smart constructor with validation
  mkEmail :: Text -> Either String Email
  mkEmail txt
    | "@" `isInfixOf` txt = Right (Email txt)
    | otherwise = Left "Email must contain @"
    
  -- Accessor function
  emailToText :: Email -> Text
  emailToText (Email t) = t
  ```

- **Phantom Types**: Use phantom types to encode additional information in the type.
  ```haskell
  -- Phantom type for file access permissions
  data Permission = Read | Write | ReadWrite
  
  newtype File (p :: Permission) = File FilePath
  
  -- Operations that respect permissions
  readFile :: File p -> IO String
  readFile (File path) = -- ...
  
  writeFile :: File 'Write -> String -> IO ()
  writeFile (File path) content = -- ...
  
  withWritableFile :: File 'Read -> (File 'Write -> IO a) -> IO a
  ```

## Module Organization and API Design

### Clean Module Structure

- **Hierarchical Module Structure**: Organize modules hierarchically (e.g., `App.Module.Submodule`) to make the codebase easier to navigate.
  ```
  MyApp/
    Core.hs         -- Core functionality
    Core/           -- Implementation details
      Types.hs
      Operations.hs
    Database.hs     -- Database facade
    Database/       -- Database implementations
      MySQL.hs
      PostgreSQL.hs
  ```

- **Facade Modules**: Create facade modules that re-export functionality from specialized modules. This allows implementation changes without affecting users of your API.
  ```haskell
  -- Database.hs (facade module)
  module Database 
    ( Connection
    , QueryResult
    , connect
    , disconnect
    , query
    ) where
  
  import Database.Internal.Types
  import Database.Internal.Connection
  import Database.Internal.Query
  ```

- **Re-export Pattern**: Use selective re-exports to create a clean, focused API while hiding implementation details.
  ```haskell
  -- Clear separation between public API and internal details
  module MyLib
    ( -- * Core Types
      Widget(..)
    , WidgetId
      -- * Widget Creation
    , createWidget
    , defaultWidget
      -- * Widget Operations
    , updateWidget
    , renderWidget
    ) where
    
  import MyLib.Internal.Types
  import MyLib.Internal.Operations
  ```

### API Design for Human Understanding

- **Module Documentation**: Begin each module with a comprehensive Haddock comment that explains its purpose, main concepts, and usage examples.
  ```haskell
  {-|
  Module      : Data.Parser
  Description : Parser combinators for structured data
  
  This module provides parser combinators for processing structured data.
  It supports:
  
  * Basic parsers for primitive types
  * Combinators for sequence and choice
  * Error reporting with context
  
  Example usage:
  
  @
  parseJSON :: String -> Either ParseError JSONValue
  parseJSON input = runParser jsonValue input
  @
  -}
  ```

- **Function Grouping**: Group related functions together and use Haddock section headers to organize the module documentation.
  ```haskell
  -- | Core data types
  
  -- | @Widget@ represents a UI element
  data Widget = ...
  
  -- | Operations on widgets
  
  -- | Create a new widget
  createWidget :: WidgetConfig -> Widget
  
  -- | Update widget properties
  updateWidget :: Widget -> WidgetUpdate -> Widget
  ```

- **Type Signatures as Documentation**: Write expressive type signatures that communicate intent. Use meaningful type and function names.
  ```haskell
  -- Less clear
  process :: [a] -> [(a, b)] -> [b] -> [c]
  
  -- More clear
  reconcileInventory :: [Product] -> [(Product, Quantity)] -> [Adjustment] -> [StockChange]
  ```

## Advanced Type System Features for Safety and Clarity

### Type Classes and Constraints

- **Type Class Constraints**: Use type class constraints to make requirements explicit and enable polymorphism.
  ```haskell
  -- Generic function that works with any monoid
  combineAll :: Monoid a => [a] -> a
  combineAll = foldr (<>) mempty
  
  -- Use with different monoid instances
  sumAll :: [Int] -> Int
  sumAll = getSum . combineAll . map Sum
  
  concatAll :: [[a]] -> [a]
  concatAll = combineAll  -- Works because lists are monoids
  ```

- **Constraint Type Aliases**: Use ConstraintKinds to create aliases for common constraint combinations.
  ```haskell
  {-# LANGUAGE ConstraintKinds #-}
  
  -- Alias for common constraint combination
  type Serializable a = (ToJSON a, FromJSON a, Show a, Eq a)
  
  -- Simplified type signature
  storeEntity :: Serializable a => Connection -> a -> IO ()
  storeEntity conn entity = do
    let json = toJSON entity
    -- Store the entity...
  ```

- **Multi-Parameter Type Classes**: Use MPTCs with functional dependencies or associated types to express relationships between types.
  ```haskell
  {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
  
  -- Repository pattern with type safety
  class Repository r e | r -> e where
    save :: e -> r -> IO r
    findById :: Id e -> r -> IO (Maybe e)
    delete :: Id e -> r -> IO r
  
  -- Implementation for specific entity type
  instance Repository UserRepo User where
    save user repo = -- Implementation
    findById userId repo = -- Implementation
    delete userId repo = -- Implementation
  ```

### Advanced Type Safety Features

- **GADTs**: Use Generalized Algebraic Data Types to enforce invariants at the type level.
  ```haskell
  {-# LANGUAGE GADTs, DataKinds #-}
  
  -- Status for a request
  data Status = Pending | Approved | Rejected
  
  -- GADT that ensures status-specific operations
  data Request s where
    PendingRequest :: RequestId -> UserData -> Request 'Pending
    ApprovedRequest :: RequestId -> UserData -> ApproverInfo -> Request 'Approved
    RejectedRequest :: RequestId -> UserData -> RejectReason -> Request 'Rejected
  
  -- Type-safe operations
  approve :: ApproverInfo -> Request 'Pending -> Request 'Approved
  approve approver (PendingRequest id userData) = 
    ApprovedRequest id userData approver
    
  -- Won't compile:
  -- approve :: ApproverInfo -> Request 'Rejected -> Request 'Approved
  ```

- **Phantom Types**: Use phantom types to add type-level tags without runtime overhead.
  ```haskell
  {-# LANGUAGE RankNTypes, KindSignatures #-}
  
  -- Phantom type for validation state
  data Validated
  data Unvalidated
  
  -- Email with validation state in the type
  newtype Email (s :: Type) = Email Text
  
  -- Smart constructor that returns validated email
  validateEmail :: Email Unvalidated -> Either String (Email Validated)
  validateEmail (Email txt)
    | "@" `isInfixOf` txt = Right (Email txt)
    | otherwise = Left "Invalid email address"
  
  -- Only validated emails can be sent
  sendEmail :: Email Validated -> Message -> IO ()
  sendEmail (Email addr) msg = -- Implementation
  ```

- **Type Families**: Use type families to compute types based on other types.
  ```haskell
  {-# LANGUAGE TypeFamilies #-}
  
  -- Type family for result of an operation based on input type
  type family ResultOf a where
    ResultOf String = Int
    ResultOf Int = Double
    ResultOf (Maybe a) = Maybe (ResultOf a)
  
  -- Function with type that depends on input
  process :: a -> ResultOf a
  process = -- Implementation
  ```

### Expressive Deriving Mechanisms

- **DerivingVia**: Use DerivingVia for zero-boilerplate reuse of implementations.
  ```haskell
  {-# LANGUAGE DerivingVia, DerivingStrategies #-}
  
  -- Newtype wrapper for JSON serialization customization
  newtype UserName = UserName Text
    deriving stock (Show, Eq)
    deriving newtype (Semigroup, Monoid)
    deriving (ToJSON, FromJSON) via Text
  
  -- Composition of deriving strategies
  newtype UserId = UserId Int
    deriving stock (Show, Eq, Ord)
    deriving (ToJSON, FromJSON) via (Tagged "id" Int)
  ```

- **DerivingStrategies**: Be explicit about deriving mechanisms for clarity.
  ```haskell
  -- Explicitly specify deriving strategy
  data User = User
    { userId :: UserId
    , userName :: UserName
    , userEmail :: Email Validated
    }
    deriving stock (Show, Eq)
    deriving anyclass (ToJSON, FromJSON)
    deriving (Semigroup) via (GenericSemigroup User)
  ```

## Composition Patterns for Readability

### Kleisli Composition for Monadic Pipelines

Kleisli composition elegantly chains monadic operations, improving readability for complex workflows.

```haskell
import Control.Arrow ((>>>), (<<<), Kleisli(..), runKleisli)

-- Monadic functions (error handling, IO, etc.)
validateInput :: Input -> Either Error ValidInput
processData :: ValidInput -> Either Error ProcessedData
generateReport :: ProcessedData -> Either Error Report

-- Create Kleisli arrows for these functions
validateK = Kleisli validateInput
processK = Kleisli processData
reportK = Kleisli generateReport

-- Compose them into a clean pipeline
pipeline :: Kleisli (Either Error) Input Report
pipeline = validateK >>> processK >>> reportK

-- Run the pipeline
processBatch :: [Input] -> [Either Error Report]
processBatch inputs = map (runKleisli pipeline) inputs

-- Compare to nested approach:
processManually :: Input -> Either Error Report
processManually input = do
  validInput <- validateInput input
  processed <- processData validInput  
  generateReport processed  -- Less clear for complex pipelines
```

### Function Composition for Pure Pipelines

When working with pure functions, standard function composition offers clarity.

```haskell
-- Pure data transformations
normalize :: RawData -> NormalizedData
analyze :: NormalizedData -> AnalysisResult 
format :: AnalysisResult -> FormattedOutput

-- Direct composition
pipeline :: RawData -> FormattedOutput
pipeline = format . analyze . normalize

-- Data flows from right to left, which can be counterintuitive

-- Alternative: Forward composition with Data.Function
import Data.Function ((&))

pipeline' :: RawData -> FormattedOutput
pipeline' data = data 
  & normalize  -- First step
  & analyze    -- Second step
  & format     -- Final step
```

## Error Handling Patterns

### Typed Errors with Monad Transformers

Use explicit error types and monad transformers for comprehensive error handling.

```haskell
-- Define a clear error hierarchy
data AppError
  = FileSystemError FilePath IOError
  | ConfigError String
  | NetworkError ConnectionInfo String
  | ValidationError [String]
  | PermissionError UserId Resource
  deriving (Show, Eq)

-- Application monad with built-in error handling
type AppM a = ReaderT AppConfig (ExceptT AppError IO) a

-- Helper for running the monad stack
runAppM :: AppConfig -> AppM a -> IO (Either AppError a)
runAppM config action = runExceptT (runReaderT action config)

-- Convert IO exceptions to domain-specific errors
safeFileOperation :: FilePath -> AppM ByteString
safeFileOperation path = do
  result <- liftIO $ try $ readFile path
  case result of
    Left e -> throwError $ FileSystemError path e
    Right content -> return content
```

### Railway-Oriented Programming with Either

Use Either for explicit error handling in pure code without the complexity of monad transformers.

```haskell
-- Define error types
data ValidationError = 
    MissingField String 
  | InvalidFormat String String
  | OutOfRange String Int Int Int
  deriving (Show, Eq)

-- Input validation function returning Either
validateInput :: UserInput -> Either ValidationError ValidatedInput
validateInput input = do
  name <- validateName (inputName input)
  age <- validateAge (inputAge input)
  email <- validateEmail (inputEmail input)
  pure ValidatedInput
    { validName = name
    , validAge = age
    , validEmail = email
    }

-- Simple validation function
validateAge :: Maybe Int -> Either ValidationError Int
validateAge Nothing = Left (MissingField "age")
validateAge (Just age)
  | age < 18 = Left (OutOfRange "age" age 18 120)
  | age > 120 = Left (OutOfRange "age" age 18 120)
  | otherwise = Right age
```

### Smart Constructors for Validation

Use smart constructors to ensure valid data at the type level.

```haskell
-- Define a newtype with private constructor
module Email (Email, mkEmail, emailToText) where

newtype Email = Email { _unEmail :: Text } -- Private constructor

-- Smart constructor returns Either for explicit error handling
mkEmail :: Text -> Either EmailError Email
mkEmail txt
  | T.null txt = Left EmailEmpty
  | not ("@" `T.isInfixOf` txt) = Left EmailMissingAt
  | not (hasDomainPart txt) = Left EmailInvalidDomain
  | otherwise = Right (Email $ T.toLower txt)
  
-- Safe access functions  
emailToText :: Email -> Text
emailToText (Email t) = t

-- Because Email constructor is not exported, all Email values in your
-- program are guaranteed to be valid
```

### Nested Error Handling with MonadError

Use MonadError for cleaner nested error handling.

```haskell
import Control.Monad.Except

-- Function signatures are cleaner with constraints instead of concrete types
processTransaction :: (MonadError AppError m, MonadIO m) => Transaction -> m Receipt
processTransaction tx = do
  -- validate will throw an error on invalid transaction
  validTx <- validate tx
  
  -- attempt to process, may throw network error
  result <- processPayment validTx `catchError` \e -> 
    -- Add context to the error
    throwError $ PaymentError (transactionId tx) e
    
  -- generate receipt if successful
  generateReceipt tx result
```

## Resource Management and Safety Patterns

### Bracket Pattern for Resource Safety

Use the bracket pattern to ensure resources are properly acquired and released even when exceptions occur.

```haskell
import Control.Exception (bracket)
import System.IO

-- Generic template for resource handling
withResource :: IO a         -- acquire resource
             -> (a -> IO ()) -- release resource
             -> (a -> IO b)  -- use resource
             -> IO b
withResource acquire release use = bracket acquire release use

-- Example: File handling with automatic cleanup
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode = bracket 
  (openFile path mode)  -- acquire
  hClose                -- release
  
-- Example: Database connection with transaction support
withTransaction :: Connection -> (Connection -> IO a) -> IO a
withTransaction conn action = bracket
  (do beginTransaction conn; return conn)  -- start transaction
  (\c -> do rollback c; return ())         -- rollback on exception
  (\c -> do result <- action c             -- run action
            commit c                       -- commit on success
            return result)
```

### Resource Management with ResourceT

For complex resource management scenarios, ResourceT from the resourcet package provides more flexibility.

```haskell
import Control.Monad.Trans.Resource

-- Create a computation that allocates and automatically frees resources
complexOperation :: ResourceT IO Result
complexOperation = do
  -- Register resources with cleanup actions
  (dbReleaseKey, dbConn) <- allocate 
    (connectDB "database.db")  -- acquire
    disconnectDB              -- release
  
  (fileReleaseKey, fileHandle) <- allocate
    (openFile "output.txt" WriteMode)  -- acquire
    hClose                            -- release
  
  -- Early release if needed
  release dbReleaseKey  
  
  -- Resources automatically released when ResourceT exits
  liftIO $ processWithResources dbConn fileHandle

-- Run the ResourceT computation
runResourceOperation :: IO Result
runResourceOperation = runResourceT complexOperation
```

### Capability-Based Security

Use the capability pattern to restrict access to sensitive operations.

```haskell
-- Define capability tokens
newtype FileReadCap = FileReadCap { allowedDirs :: [FilePath] } 
newtype FileWriteCap = FileWriteCap { writeDirs :: [FilePath] }

-- Operations require explicit capabilities
readFile' :: FileReadCap -> FilePath -> IO String
readFile' cap path = do
  -- Verify path is in allowed directories
  allowed <- isPathAllowed (allowedDirs cap) path
  if allowed 
    then readFile path
    else throwIO $ PermissionError $ "Cannot read: " ++ path
    
-- Restricted capability creation
rootCap :: IO FileReadCap
rootCap = do
  -- Check if user has admin rights
  isAdmin <- checkAdminRights
  if isAdmin
    then return $ FileReadCap ["/"]  -- Full access
    else return $ FileReadCap ["/home/user"]  -- Limited access
```

### Resource Pools for Performance

Use resource pooling for expensive resources like database connections.

```haskell
import Data.Pool

-- Create a connection pool
initConnectionPool :: Config -> IO (Pool Connection)
initConnectionPool config = createPool
  (connect (dbHost config) (dbUser config))  -- create resource
  close                                     -- destroy resource
  1           -- stripes (for concurrency)
  60          -- unused resource timeout (seconds)
  10          -- maximum resources per stripe

-- Use a resource from the pool
withConnection :: Pool Connection -> (Connection -> IO a) -> IO a
withConnection pool action = withResource pool action
```

## Testing Patterns for Robust Code

### Property-Based Testing

Use property-based testing to identify edge cases that unit tests might miss.

```haskell
import Test.QuickCheck
import Data.List (sort)

-- Define properties that should hold for any input
prop_reverseInvolutive :: [Int] -> Bool
prop_reverseInvolutive xs = reverse (reverse xs) == xs

prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs = sort (sort xs) == sort xs

-- Test invariants that your functions should maintain
prop_parseRenderRoundtrip :: Config -> Property
prop_parseRenderRoundtrip config = 
  parseConfig (renderConfig config) === Just config

-- Run the tests
main :: IO ()
main = do
  quickCheck prop_reverseInvolutive
  quickCheck prop_sortIdempotent
  quickCheck prop_parseRenderRoundtrip
```

### Isolated Test Environments

Create isolated, reproducible test environments for reliable testing.

```haskell
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

-- Helper for setting up a test environment
withTestEnvironment :: (FilePath -> IO ()) -> IO ()
withTestEnvironment runTest = 
  withSystemTempDirectory "test-dir" $ \tmpDir -> do
    -- Create test files and directories
    createDirectoryIfMissing True (tmpDir </> "src")
    createDirectoryIfMissing True (tmpDir </> "config")
    writeFile (tmpDir </> "src" </> "test.file") "test content"
    writeFile (tmpDir </> "config" </> "settings.json") "{\"mode\":\"test\"}"
    
    -- Run the test with the prepared environment
    runTest tmpDir

-- Use it in hspec tests
it "processes files correctly" $
  withTestEnvironment $ \tmpDir -> do
    -- Configure app to use the temp directory
    let config = defaultConfig { rootDir = tmpDir }
    
    -- Run the application
    result <- runApp config
    
    -- Make assertions
    result `shouldBe` Success
```

### Golden Tests for Output Verification

Use golden testing to verify your outputs match expected templates.

```haskell
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy as BL

-- Test that generated output matches a "golden" file
goldenOutputTest :: TestTree
goldenOutputTest = goldenVsString
  "report generation"                         -- test name
  "test/golden/expected_report.json"          -- golden file path
  (BL.fromStrict <$> generateReport testData) -- actual output

-- For complex outputs like HTML, use a difference tool
htmlGoldenTest :: TestTree
htmlGoldenTest = goldenVsFileDiff
  "page rendering"              -- test name
  diffCommand                   -- diff command to use
  "test/golden/expected.html"   -- golden file
  "test/output/actual.html"     -- actual output file
  (renderPage testData)         -- action to generate actual output
  where
    diffCommand ref new = ["diff", "-u", ref, new]
```

### Table-Driven Testing

Use table-driven testing for testing multiple related cases concisely.

```haskell
import Test.Hspec

-- Define test cases as a list of input-output pairs
testCases :: [(String, Int)]
testCases = 
  [ ("123", 123)
  , ("0", 0)
  , ("00123", 123)
  , ("+123", 123)
  , ("-123", -123)
  ]

-- Test all cases using the same pattern
spec :: Spec
spec = describe "parseNumber" $ do
  forM_ testCases $ \(input, expected) ->
    it ("parses " ++ show input ++ " correctly") $ do
      parseNumber input `shouldBe` Right expected

-- For more complex test cases, use records
data ValidationTestCase = ValidationTestCase
  { testName :: String
  , testInput :: UserInput
  , expectedResult :: Either ValidationError ValidatedInput
  }

validationTests :: [ValidationTestCase]
validationTests = 
  [ ValidationTestCase 
      "valid input" 
      (UserInput "John" (Just 30) "john@example.com")
      (Right $ ValidatedInput "John" 30 "john@example.com")
  , ValidationTestCase
      "missing age"
      (UserInput "John" Nothing "john@example.com")
      (Left $ MissingField "age")
  ]
```

### Test Fixtures and Mocks

Use fixtures and mocks to test code that depends on external systems.

```haskell
-- Define a typecalss for database operations
class Monad m => MonadDB m where
  queryUsers :: m [User]
  saveUser :: User -> m ()
  
-- Production implementation
instance MonadDB IO where
  queryUsers = queryUsersFromDatabase
  saveUser = saveUserToDatabase
  
-- Test implementation 
instance MonadDB (State TestDB) where
  queryUsers = gets testDBUsers
  saveUser user = modify $ \db -> 
    db { testDBUsers = user : testDBUsers db }

-- Example test
it "creates user profile" $ do
  -- Set up initial DB state
  let initialDB = TestDB { testDBUsers = [] }
  
  -- Run operation with mock DB
  let (result, finalDB) = runState createUserProfile initialDB
  
  -- Assert the operation worked correctly
  length (testDBUsers finalDB) `shouldBe` 1
  
-- Function being tested uses constraint for testability
createUserProfile :: MonadDB m => m User
createUserProfile = do
  -- Implementation
```

## Debugging and Maintainability Patterns

### Function Decomposition for Testability

Break complex functions into smaller, testable parts that can be individually verified.

```haskell
-- Original monolithic function (hard to test and debug)
complexProcess :: Config -> [Input] -> IO [Output]
complexProcess config inputs = do
  -- 100+ lines of complex logic with multiple responsibilities
  -- and many potential failure points...

-- Refactored into testable components
validateInputs :: [Input] -> Either ValidationError [ValidInput]
validateInputs = traverse validateSingleInput

processValidInputs :: Config -> [ValidInput] -> IO [ProcessedData]
processValidInputs config = traverse (processOne config)  

generateOutputs :: [ProcessedData] -> [Output]
generateOutputs = map convertToOutput

-- Compose them back together with clear error handling
complexProcess :: Config -> [Input] -> IO (Either Error [Output])
complexProcess config inputs = do
  case validateInputs inputs of
    Left validationError -> 
      pure $ Left $ ValidationFailed validationError
      
    Right validInputs -> do
      processResult <- try $ processValidInputs config validInputs
      case processResult of
        Left ex -> 
          pure $ Left $ ProcessingFailed ex
          
        Right processed ->
          pure $ Right $ generateOutputs processed
```

### Layered Debugging Techniques

Use a combination of tracing approaches for effective debugging.

```haskell
import Debug.Trace (trace, traceShowId, traceM)
import qualified System.IO as IO

-- 1. Simple trace for basic logging (but doesn't clutter production code)
withTracing :: Bool -> a -> String -> a
withTracing True x msg = trace msg x
withTracing False x _ = x

-- 2. Effectful tracing for debugging monadic code
processItems :: [Item] -> IO [Result]
processItems = mapM $ \item -> do
  when debugMode $ traceM $ "Processing: " ++ show item
  result <- processItem item
  when debugMode $ traceM $ "Result: " ++ show result
  return result

-- 3. Conditional file logging when trace output is too large
logToFile :: String -> IO ()
logToFile msg = when debugMode $
  IO.appendFile "debug.log" (msg ++ "\n")
  
-- 4. TraceShowId for quick inspection of values in a pipeline
calculateResults :: [Input] -> [Output]
calculateResults = filter isValid 
                   >>> map preprocess 
                   >>> traceShowId  -- See the values mid-pipeline
                   >>> map calculate
                   >>> filter isSignificant

-- 5. Temporary function modification for deeper inspection
-- Original function
process :: Item -> Result
process = step1 >>> step2 >>> step3

-- Modified during debugging
process :: Item -> Result
process item = 
  let s1 = step1 item
      _ = trace ("After step1: " ++ show s1) ()
      s2 = step2 s1
      _ = trace ("After step2: " ++ show s2) ()
  in step3 s2
```

### Typed Holes for Guided Development

Use typed holes to let the compiler guide your implementation.

```haskell
-- Start with the function type signature
processTransaction :: UserId -> Transaction -> Either Error Receipt
processTransaction userId transaction = _implementThis

-- The compiler will tell you the expected type of _implementThis

-- Gradually fill in implementation guided by holes
processTransaction userId transaction = do
  user <- _getUser userId
  validated <- _validateTransaction user transaction
  _processPayment validated

-- Each hole tells you what you need to implement next
_getUser :: UserId -> Either Error User
_getUser = ...

_validateTransaction :: User -> Transaction -> Either Error ValidatedTransaction
_validateTransaction = ...
```

### Equational Reasoning and Step-by-Step Refactoring

Use equational reasoning to verify code transformations.

```haskell
-- Original code
sum (map square xs)

-- Step 1: Rewrite using function composition
sum . map square $ xs

-- Step 2: Introduce a specialized function
sumOfSquares = sum . map square

-- Verification:
-- sum (map square xs)
-- = sum . map square $ xs  -- By function composition
-- = sumOfSquares xs        -- By definition

-- More complex example:
processList xs = filter p1 (map f (filter p2 xs))

-- Transform step by step:
processList xs = (filter p1 . map f . filter p2) xs

-- Extract function:
processList = filter p1 . map f . filter p2

-- Each step preserves behavior but improves readability
```

## Performance Patterns and Optimizations

### Efficient ByteString Usage

Use ByteString for efficient text and binary data handling.

```haskell
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

-- Converting between ByteString and String (avoid in performance-critical code)
stringToBS :: String -> BS.ByteString
stringToBS = BS8.pack  -- For ASCII-only text

-- For general Unicode text, use Text instead of String/ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

textToBS :: T.Text -> BS.ByteString
textToBS = TE.encodeUtf8

bsToText :: BS.ByteString -> Either String T.Text
bsToText bs = case TE.decodeUtf8' bs of
  Left err -> Left $ "UTF-8 decoding error: " ++ show err
  Right text -> Right text

-- Efficient file reading
readLargeFile :: FilePath -> IO BL.ByteString
readLargeFile = BL.readFile  -- Lazy reading for large files

-- Stream processing for large data
processLargeFile :: FilePath -> FilePath -> IO ()
processLargeFile input output = do
  contents <- BL.readFile input
  BL.writeFile output $ BL.filter (/= 0) contents
```

### Strict Fields for Memory Efficiency

Use strictness annotations to avoid space leaks.

```haskell
-- Without strictness, can cause space leaks
data Configuration = Configuration
  { configPort :: Int
  , configHost :: String
  , configTimeout :: Int
  }

-- With strictness annotations, more memory-efficient
data Configuration' = Configuration'
  { configPort' :: !Int  -- Strict field
  , configHost' :: !String
  , configTimeout' :: !Int
  }

-- Strictness and UNPACK for numeric data
data Point = Point
  { x :: {-# UNPACK #-} !Double  -- Unpacked strict field
  , y :: {-# UNPACK #-} !Double
  }

-- For record types with many fields
{-# LANGUAGE StrictData #-}  -- All fields strict by default
data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  }
```

### Fusion and Deforestation

Take advantage of list fusion to eliminate intermediate data structures.

```haskell
-- This will create an intermediate list
naiveProcess :: [Int] -> Int
naiveProcess xs = sum (filter even (map (*2) xs))

-- GHC can optimize this with list fusion
fusedProcess :: [Int] -> Int
fusedProcess = sum . filter even . map (*2)

-- Even better: use foldr to fuse everything into a single pass
singlePassProcess :: [Int] -> Int
singlePassProcess = foldr (\x acc -> if even (x*2) then acc + (x*2) else acc) 0

-- For more control, use a specialized streaming library
import qualified Streamly.Prelude as S

streamProcess :: [Int] -> IO Int
streamProcess xs = S.fold S.sum 
                 $ S.filter even 
                 $ S.map (*2) 
                 $ S.fromList xs
```

### Lazy vs. Strict Evaluation Control

Explicitly control evaluation strategy for better performance.

```haskell
import Control.DeepSeq (NFData, force, ($!!))

-- Force full evaluation of a structure when needed
processStrictly :: (NFData a) => [a] -> [a]
processStrictly xs = force (map process xs)

-- Manually force evaluation to specific depth
data Tree a = Leaf a | Node (Tree a) (Tree a)

forceTree :: Tree a -> ()
forceTree (Leaf _) = ()
forceTree (Node l r) = forceTree l `seq` forceTree r `seq` ()

-- Use bang patterns for strict evaluation in function arguments
sumListStrict :: [Int] -> Int
sumListStrict !xs = sum xs  -- Force evaluation of xs

-- Use BangPatterns language extension for more control
{-# LANGUAGE BangPatterns #-}

foldlStrict :: (b -> a -> b) -> b -> [a] -> b
foldlStrict f !acc [] = acc
foldlStrict f !acc (x:xs) = foldlStrict f (f acc x) xs
```

## Build and Packaging Best Practices

### Cabal Configuration

Properly configure your Cabal file for reliable builds and distribution.

```haskell
-- Example cabal file structure with key sections
name:                my-project
version:             0.1.0
synopsis:            Short description of your project
description:         Longer, multi-line description
                     of your project's purpose and features.
license:             MIT
license-file:        LICENSE
author:              Your Name
maintainer:          your.email@example.com
category:            Development
build-type:          Custom  -- Use Custom for custom Setup.hs
cabal-version:       2.0

-- Set up custom build if needed
custom-setup
  setup-depends:     base >= 4.7 && < 5,
                     Cabal >= 2.0.0.2 && < 3.12,
                     directory,
                     filepath,
                     process

-- Documentation files that should be included in source distributions
extra-source-files:  README.md
                     CHANGELOG.md
                     examples/*.hs

-- Files to be installed with the package
data-files:          templates/*.txt
                     data/*.json

-- Create an autogenerated module for accessing data-files
auto-generated-modules: Paths_my_project

-- Library component
library
  -- Modules exposed to users of the library
  exposed-modules:     MyProject
                       MyProject.Core
                       MyProject.Types
  
  -- Internal modules not exposed to users
  other-modules:       MyProject.Internal.Util
                       Paths_my_project  -- Auto-generated module for data files
  
  -- Enable useful warnings
  ghc-options:         -Wall 
                       -Wcompat
                       -Wincomplete-record-updates
                       -Wincomplete-uni-patterns
                       -Wredundant-constraints
  
  -- Dependencies with version constraints
  build-depends:       base >= 4.7 && < 5,
                       aeson >= 1.4 && < 2.2,
                       text >= 1.2 && < 2.1,
                       containers >= 0.6 && < 0.7
```

### Version Range Best Practices

Specify appropriate version ranges for dependencies to avoid compatibility issues.

```
-- For most dependencies, specify both lower and upper bounds
build-depends: base >= 4.14 && < 5,
               text >= 1.2.4 && < 2.1,
               aeson >= 2.0 && < 2.2

-- Version range notation examples:
-- == 1.0.0         -- Exactly version 1.0.0
-- >= 1.0 && < 1.1  -- Greater than or equal to 1.0 and less than 1.1
-- ^>= 1.0.0        -- Compatible with version 1.0.0 (>=1.0.0 && <1.1)
-- ~> 1.0.0         -- Similar to ^>= but with more restriction
```

### Documentation Integration

Integrate documentation into your build process for better user experience.

```haskell
-- In Setup.hs
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = \args buildFlags pkg lbi -> do
      -- Run standard post-build first
      postBuild simpleUserHooks args buildFlags pkg lbi
      
      -- Generate documentation
      generateDocs pkg lbi
  }

-- Generate embedded documentation from Markdown
generateDocs :: PackageDescription -> LocalBuildInfo -> IO ()
generateDocs pkg lbi = do
  let docsDir = buildDir lbi </> "docs"
  createDirectoryIfMissing True docsDir
  
  -- Process each documentation file
  mapM_ (processDoc docsDir) 
    ["README.md", "TUTORIAL.md", "API.md"]
  
  -- Use Haddock to generate API documentation
  let ghcProg = programPath (haddockProgram (withPrograms lbi))
      pkgDb = packageDBFlags lbi
  runProcess ghcProg ["--haddock", ...] Nothing Nothing Nothing Nothing
```

### Making Use of Flags and Conditionals

Use flags and conditional compilation for flexible builds.

```haskell
-- Define build flags in the cabal file
flag strict
  description: Enable stricter GHC options
  default:     False
  manual:      True

flag optimize
  description: Build with optimization
  default:     True
  manual:      False

-- Use flags in the build configuration
library
  if flag(strict)
    ghc-options: -Wall -Werror
  else
    ghc-options: -Wall
    
  if flag(optimize)
    ghc-options: -O2
  else
    ghc-options: -O0
    
  -- Conditional dependencies
  if os(windows)
    build-depends: Win32
  else
    build-depends: unix

-- For system-specific code
  if os(darwin)
    cpp-options: -DMACOS
    other-modules: System.MacOS.Specific
  elif os(linux)
    cpp-options: -DLINUX
    other-modules: System.Linux.Specific
```

### Custom Setup Scripts

```haskell
-- Setup.hs for custom build steps
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

main = defaultMainWithHooks simpleUserHooks
  { postBuild = \args flags pkg lbi -> do
      -- Run standard post-build first
      postBuild simpleUserHooks args flags pkg lbi
      -- Then run custom actions
      customAction args flags pkg lbi
  }

-- Custom build/install actions
customAction :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
customAction _ _ pkg lbi = do
  -- Access package configuration
  let pkgName = unPackageName $ pkgName $ package pkg
      buildDir = buildDir lbi
  
  -- Execute custom build steps
  -- ...
```

## Version Number Management

```haskell
-- Access version from cabal file
import qualified Paths_<package> as Meta
import Data.Version (showVersion)

-- Display version
version :: String
version = showVersion Meta.version
```

## Best Practices

- Favor pure Haskell implementations over shell commands
- Document system dependencies explicitly
- Load resource files from standardized locations, not hardcoded paths
- Derive version information from the cabal file, not hardcoded
- Use type applications for parametric types
- Normalize paths for cross-platform compatibility
- Add explicit type annotations for complex expressions
- Integrate with standard system conventions (man pages, config directories)
- Use Cabal's installation system rather than custom scripts for deployable artifacts

## Documentation Integration

```haskell
-- In cabal file
data-files:
  doc/*.md,           -- Source files
  templates/*.txt     -- Templates

-- In Setup.hs
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.Utils (installOrdinaryFiles)

-- Generate documentation during build
postBuild _ _ pkg lbi = do
  let dataDirName = dataDir lbi
      docSrcDir = dataDirName </> "doc"
      docDestDir = buildDir lbi </> "doc"
  
  -- Generate docs from templates
  generateDocs docSrcDir docDestDir

-- Install documentation to standard locations
copyHook oldHook pkg_descr lbi hooks flags = do
  -- First do the standard copy
  oldHook pkg_descr lbi hooks flags
  
  -- Then copy documentation to proper locations
  let docDir = case os of
        "darwin" -> "/usr/local/share/doc/" ++ pkgName
        "linux"  -> "/usr/share/doc/" ++ pkgName
        _        -> error "Unsupported OS"
  
  installOrdinaryFiles verbosity docDir [(buildDir lbi </> "doc", "*.html")]
```

## Patterns for Human-AI Collaboration

These patterns are particularly effective when working with AI assistants on Haskell projects.

### Explicit Type Annotations

Add type annotations to make intentions clear and guide AI inference, even when GHC can infer types.

```haskell
-- Without annotation (ambiguous intention)
processData input = map process . filter isValid $ input

-- With annotation (clear intention)
processData :: [InputData] -> [OutputData]
processData input = map process . filter isValid $ input

-- Intermediate type annotations for complex pipelines
processData :: [InputData] -> [OutputData]
processData input = 
  let validData = filter isValid input        :: [InputData]
      processedData = map process validData   :: [IntermediateData]
      result = finalize <$> processedData     :: [OutputData]
  in result
```

### Named Function Parameters

Use record syntax for complex parameter sets to make function usage self-documenting.

```haskell
-- Hard to understand parameter meanings
createUser :: String -> Int -> String -> Bool -> IO User
createUser name age email verified = ...

-- Parameters are self-documenting with records
data CreateUserParams = CreateUserParams
  { userName :: String
  , userAge :: Int
  , userEmail :: String
  , isVerified :: Bool
  }

createUser :: CreateUserParams -> IO User
createUser params = ...

-- Usage is clear and order-independent
newUser <- createUser CreateUserParams
  { userName = "John"
  , userAge = 30
  , userEmail = "john@example.com"
  , isVerified = True
  }
```

### Consistent Error Handling Patterns

Choose a consistent error handling approach and stick to it across the codebase.

```haskell
-- Example with ExceptT pattern
type AppM a = ExceptT AppError IO a

-- Clear error hierarchy
data AppError
  = ValidationError String
  | DatabaseError DBError
  | AuthError AuthenticationError
  | NotFoundError Resource
  deriving (Show, Eq)

-- Helper functions for error handling
whenM :: Monad m => m Bool -> m () -> m ()
whenM cond action = do
  result <- cond
  when result action

-- Usage
validateInput :: Input -> AppM ValidatedInput
validateInput input = do
  whenM (pure $ null $ inputName input) $
    throwError $ ValidationError "Name cannot be empty"
  
  whenM (pure $ inputAge input < 18) $
    throwError $ ValidationError "Must be at least 18 years old"
    
  -- Create validated input after all checks pass
  return ValidatedInput
    { validName = inputName input
    , validAge = inputAge input
    }
```

### Build System Patterns

Use simpler build configurations when possible to ease maintenance.

```haskell
-- Prefer Simple build-type over Custom when possible
build-type: Simple

-- Use common extensions across the project
default-extensions: 
  OverloadedStrings
  LambdaCase
  NamedFieldPuns
  RecordWildCards
  DeriveGeneric
  DeriveDataTypeable

-- Only use custom Setup.hs when actually needed
-- For example, to generate and install man pages
```

### Module Organization for Discovery

Structure modules to facilitate code discovery by humans and AI assistants.

```haskell
-- Organize hierarchically with explicit exports
module MyApp
  ( -- * Core types
    AppConfig(..)
  , AppState(..)
    
    -- * Running the application
  , runApp
  , runAppWithConfig
    
    -- * Error handling
  , AppError(..)
  , handleError
  ) where

-- Create index modules
module MyApp.Database
  ( -- * Re-exports from all database modules
    module MyApp.Database.Connection
  , module MyApp.Database.Query
  , module MyApp.Database.Migration
  ) where

import MyApp.Database.Connection
import MyApp.Database.Query
import MyApp.Database.Migration

-- Function purpose is clear from name
validateUserInput :: UserInput -> Either ValidationError ValidatedInput
```

### Type-Level Documentation

Embed information in types to make function behavior self-documenting.

```haskell
-- Types convey information about the function's behavior
authRequired :: HasAuth r => RIO r Resource
adminOnly :: HasAdminAccess r => RIO r Resource

-- Status-tracking in result type
data VerificationStatus = Pending | Verified | Rejected
data Email (s :: VerificationStatus) = Email Text

sendEmail :: Email 'Verified -> Message -> IO ()

-- Directional data flow
data Input
data Processed
data Output

data Pipeline s a where
  Input :: a -> Pipeline 'Input a
  Process :: Pipeline 'Input a -> Pipeline 'Processed a
  Output :: Pipeline 'Processed a -> Pipeline 'Output a

-- Function chains are guaranteed correct order by types
pipeline :: Data -> Pipeline 'Output Result
pipeline = Output . Process . Input
```