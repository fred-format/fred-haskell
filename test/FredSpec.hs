{-# LANGUAGE OverloadedStrings #-}

module FredSpec
    ( spec
    )
where

import           Test.Hspec
import           Control.Monad
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.List
import           Data.HashMap.Strict           as HashMap
import           System.Directory
import           System.Process
import qualified System.IO.Strict              as SIO
import qualified Data.Text                     as T
import           Data.Char
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Char8         as BC
import           Fred
import           Fred.Value                     ( FredDocument(..)
                                                , FredValue(..)
                                                , FredAtom(..)
                                                )
instance ToJSON FredDocument where
    toJSON (Stream fredDoc  ) = toJSON fredDoc

    toJSON (Doc    fredValue) = toJSON fredValue

instance ToJSON FredValue where
    toJSON (Tag (tag, [], atom)) =
        object ["tag" .= tag, "meta" .= Null, "value" .= toJSON atom]

    toJSON (Tag (tag, metaData, atom)) =
        object ["tag" .= tag, "meta" .= metaData, "value" .= toJSON atom]

    toJSON (NonTag atom) =
        object ["tag" .= Null, "meta" .= Null, "value" .= toJSON atom]

instance ToJSON FredAtom where
    toJSON (A arrayAtom )            = toJSON arrayAtom
    toJSON (O objectAtom)            = toJSON objectAtom
    toJSON NULL                      = "null"
    toJSON (B bool) = toJSON $ Data.List.map toLower (show bool)
    toJSON (S         str          ) = toJSON str
    toJSON (N         (Left  int  )) = toJSON $ show int
    toJSON (N         (Right float)) = toJSON $ show float
    toJSON (Symbol    str          ) = toJSON str
    toJSON (Blob      str          ) = toJSON (BC.unpack str)
    toJSON (LDate     day          ) = toJSON day
    toJSON (LTime     time         ) = toJSON time
    toJSON (LDateTime localTime    ) = toJSON localTime
    toJSON (DateTime  zonedTime    ) = toJSON zonedTime

toPair :: (String, FredAtom) -> (T.Text, Value)
toPair (name, value) = (T.pack name, toJSON value)


spec :: Spec
spec = do
    describe "Valid fred documents"   validTests
    describe "Invalid fred documents" invalidTests


validTests :: Spec
validTests = do
    tempDir <- runIO getTemporaryDirectory
    runIO (setCurrentDirectory tempDir)
    runIO cloneGitTests
    dirs <- runIO (listDirectory "./fred-test/tests/valid/")
    forM_
        dirs
        (\dir -> context dir $ do
            examples <- runIO (readValidExamples dir)
            forM_
                examples
                (\(name, (input, output)) -> it name $ do
                    strIn  <- input
                    strOut <- output
                    getFred strIn `shouldBe` (Right strOut)
                )
        )

getFred :: String -> Either String String
getFred input = case Fred.parse input of
    Left  parseError -> Left "Error"
    Right result     -> Right $ BL.unpack $ encodePretty result


readValidExamples :: String -> IO [(String, (IO String, IO String))]
readValidExamples dir = do
    testDir <- makeAbsolute ("./fred-test/tests/valid/" ++ dir)
    withCurrentDirectory
        testDir
        (do
            currentDir <- getCurrentDirectory

            files      <- listDirectory currentDir
            let list = createValidTests currentDir files
            pure list
        )


createValidTests :: String -> [String] -> [(String, (IO String, IO String))]
createValidTests dir = Data.List.foldl transformToTest []
  where
    transformToTest acc fileName =
        case HashMap.lookup filePrefix (HashMap.fromList acc) of
            Nothing -> HashMap.toList $ HashMap.insert
                filePrefix
                ((input dir), (output dir))
                (HashMap.fromList acc)
            Just _ -> acc
      where
        filePrefix = takeWhile (/= '.') fileName
        input dir = readFile (dir ++ "/" ++ filePrefix ++ ".fred")
        output dir = readFile (dir ++ "/" ++ filePrefix ++ ".json")


invalidTests :: Spec
invalidTests = do
    dirs <- runIO (listDirectory "./fred-test/tests/invalid/")
    runIO (print dirs)
    forM_
        dirs
        (\dir -> context dir $ do
            tests <- runIO (readInvalidTests dir)
            forM_
                tests
                (\(name, input) -> it name $ do
                    strIn <- input
                    getFred strIn `shouldBe` (Left "Error")
                )
        )

readInvalidTests :: String -> IO [(String, IO String)]
readInvalidTests dir = do
    testDir <- makeAbsolute ("./fred-test/tests/invalid/" ++ dir)
    withCurrentDirectory
        testDir
        (do
            currentDir <- getCurrentDirectory
            print currentDir
            files <- listDirectory currentDir
            let list = createInvalidTests currentDir files
            pure list
        )

createInvalidTests :: String -> [String] -> [(String, IO String)]
createInvalidTests dir = Data.List.foldl transformToTest []
  where
    transformToTest acc fileName =
        case HashMap.lookup filePrefix (HashMap.fromList acc) of
            Nothing -> HashMap.toList $ HashMap.insert
                filePrefix
                (input dir)
                (HashMap.fromList acc)
            Just _ -> acc
      where
        filePrefix = takeWhile (/= '.') fileName
        input dir = readFile (dir ++ "/" ++ filePrefix ++ ".fred")

cloneGitTests :: IO ()
cloneGitTests = do
    readProcess "rm" ["-rf", "./fred-test"] []
    readProcess "git"
                ["clone", "https://github.com/fred-format/fred-test.git"]
                []
    pure ()
