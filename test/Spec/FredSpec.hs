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
import           Fred.Value                     ( FredValue(..)
                                                , FredAtom(..)
                                                )
instance ToJSON FredValue where
    toJSON (Tag (tag, [], atom)) =
        object ["tag" .= tag, "meta" .= Null, "value" .= toJSON atom]

    toJSON (Tag (tag, metaData, atom)) = object
        [ "tag" .= tag
        , "meta" .= object (Data.List.map toPair metaData)
        , "value" .= toJSON atom
        ]

    toJSON (NonTag atom) = toJSON atom

instance ToJSON FredAtom where
    toJSON (A arrayAtom ) = toJSON arrayAtom
    toJSON (O objectAtom) = object
        [ "type" .= toJSON ("object" :: String)
        , ("value" .= object (Data.List.map toPair objectAtom))
        ]
    toJSON NULL              = Null
    toJSON (B bool         ) = toJSON bool
    toJSON (S str          ) = toJSON str
    toJSON (N (Left  int  )) = toJSON int
    toJSON (N (Right float)) = toJSON float

    toJSON (Symbol str) =
        object ["value" .= (toJSON str), "type" .= toJSON ("symbol" :: String)]

    toJSON (Blob str) =
        object
            [ "type" .= toJSON ("blob" :: String)
            , "value" .= toJSON (BC.unpack str)
            ]
    toJSON (LDate day) =
        object ["type" .= toJSON ("date" :: String), "value" .= toJSON day]
    toJSON (LTime time) =
        object ["type" .= toJSON ("date" :: String), "value" .= toJSON time]
    toJSON (LDateTime localTime) = object
        ["type" .= toJSON ("date" :: String), "value" .= toJSON localTime]
    toJSON (DateTime zonedTime) = object
        ["type" .= toJSON ("date" :: String), "value" .= toJSON zonedTime]


toPair :: ToJSON a => (String, a) -> (T.Text, Value)
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
    Right result     -> Right $ BL.unpack $ encodePretty' fredConfig result

fredConfig :: Config
fredConfig = Config
    { confIndent          = Spaces 4
    , confCompare         = keyOrder
                                [ "tag"
                                , "meta"
                                , "type"
                                , "value"
                                , "key"
                                , "key1"
                                , "key 1"
                                , "key 2"
                                , "name"
                                , "age"
                                ]
    , confNumFormat       = Generic
    , confTrailingNewline = False
    }


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
