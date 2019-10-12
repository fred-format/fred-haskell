{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Data.Time
import           Data.ByteString
import           Fred                           ( parse
                                                , minify
                                                )
import           Fred.Value                     ( FredValue(..)
                                                , FredAtom(..)
                                                )


genFredValue :: Gen FredValue
genFredValue = Gen.recursive
    Gen.choice
    [(NonTag <$> genFredAtom)]
    [ Tag
          <$> (   (,,)
              <$> (Gen.string (Range.linear 1 20) Gen.alpha)
              <*> (Gen.list
                      (Range.linear 0 100)
                      (   (,)
                      <$> (Gen.string (Range.constant 1 20) Gen.alpha)
                      <*> genFredAtom
                      )
                  )
              <*> (genFredValue)
              )
    ]


genFredAtom :: Gen FredAtom
genFredAtom = Gen.recursive
    Gen.choice
    [ B <$> Gen.bool
    , S <$> Gen.string (Range.linear 0 20) Gen.alpha
    , N <$> Gen.choice
        [ Left <$> toInteger <$> Gen.int (Range.linear 0 maxBound)
        , Right <$> Gen.double (Range.linearFrac 0.0 1000.0)
        ]
    , Symbol <$> Gen.string (Range.linear 1 20) Gen.alpha
    , Blob <$> Gen.filter (checkByteString)
                      (Gen.bytes (Range.linear 0 200))
    , LDate <$> genDay
    , LTime <$> genTimeOfDay
    , LDateTime <$> genLocalTime-- LocalTime Day TimeOfDay
    , DateTime <$> genZonedTime -- ZonedTime LocalTime TimeZone
    , pure NULL
    ]
    [ (   O
      <$> (Gen.list
              (Range.linear 0 100)
              (   (,)
              <$> (Gen.string (Range.constant 1 20) Gen.alpha)
              <*> genFredValue
              )
          )
      )
    , A <$> Gen.list (Range.linear 0 100) genFredValue
    ]


checkByteString :: ByteString -> Bool
checkByteString str = isInfixOf str (Data.ByteString.pack([fromInteger 96, fromInteger 92]))

genDay :: MonadGen m => m Day
genDay = do
    y <- toInteger <$> Gen.int (Range.constant 1858 3000)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 31)
    let day = fromGregorian y m d
    pure $ day

genTimeOfDay :: MonadGen m => m TimeOfDay
genTimeOfDay = do
    seconds <- toInteger <$> Gen.int (Range.linear 0 86400)
    let time = timeToTimeOfDay $ secondsToDiffTime seconds
    pure $ time

genTimeZone :: MonadGen m => m TimeZone
genTimeZone = do
    minutes <- Gen.int (Range.linear (-720) (720))
    let timezone = minutesToTimeZone minutes
    pure $ timezone

genLocalTime :: MonadGen m => m LocalTime
genLocalTime = do
    day       <- genDay
    timeOfDay <- genTimeOfDay
    let local = LocalTime day timeOfDay
    pure $ local

genZonedTime :: MonadGen m => m ZonedTime
genZonedTime = do
    day       <- genDay
    timeOfDay <- genTimeOfDay
    let local = LocalTime day timeOfDay
    timeZone <- genTimeZone
    let zoned = ZonedTime local timeZone
    pure $ zoned

prop_trip :: Property
prop_trip = property $ do
    v <- forAll genFredValue
    tripping v minify parse

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO Bool
main = do
    tests
