{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings #-}

module DataDefaultGenericsSpec where

import Data.Fixed
import Foreign.C.Types
import Data.Monoid
import Data.Ratio
import Data.Complex
import qualified Data.Set      as S
import qualified Data.Map      as M
import qualified Data.IntMap   as IM
import qualified Data.IntSet   as IS
import qualified Data.Sequence as Seq
import qualified Data.Tree     as Tree
import qualified Data.DList    as DList
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy    as HSL
import qualified Data.HashSet         as HSet
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import qualified Data.Vector.Primitive as VP
import System.Locale
import System.IO
import GHC.Generics

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Data.Default.Generics

import Test.Hspec

data TestRec = TestRec
  { bool    :: Bool
  , txt     :: T.Text
  , bs      :: BSL.ByteString
  , lst     :: [CInt]
  , ut      :: UTCTime
  } deriving (Eq,Show,Generic)

instance Default TestRec

data TestList = Cons Int TestList | Nil deriving (Eq,Show,Generic)
instance Default TestList

data TestList2 = Nil2 | Cons2 Int TestList2 deriving (Eq,Show,Generic)
instance Default TestList2

{-# ANN spec ("HLint: ignore Too strict if"::String) #-}
spec :: Spec
spec = describe "DataDefaultGenerics" $ do
  -- Boolean
  it "Bool"     $ (def :: Bool)               `shouldBe` False

  -- Int
  it "Int"      $ (def :: Int)            `shouldBe` 0
  it "Int8"     $ (def :: Int)            `shouldBe` 0
  it "Int16"    $ (def :: Int)            `shouldBe` 0
  it "Int32"    $ (def :: Int)            `shouldBe` 0
  it "Int64"    $ (def :: Int)            `shouldBe` 0
  it "Word"     $ (def :: Int)            `shouldBe` 0
  it "Word8"    $ (def :: Int)            `shouldBe` 0
  it "Word16"   $ (def :: Int)            `shouldBe` 0
  it "Word32"   $ (def :: Int)            `shouldBe` 0
  it "Word64"   $ (def :: Int)            `shouldBe` 0
  it "Integer"  $ (def :: Int)            `shouldBe` 0
  it "Float"    $ (def :: Float)          `shouldBe` 0
  it "Double"   $ (def :: Double)         `shouldBe` 0
  it "Ratio a"  $ (def :: Ratio Int)      `shouldBe` 0
  it "Complex a"$ (def :: Complex Double) `shouldBe` 0 :+ 0

  -- C Interface
  it "CChar"    $ (def :: CChar)    `shouldBe` 0
  it "CSChar"   $ (def :: CSChar)   `shouldBe` 0
  it "CUChar"   $ (def :: CUChar)   `shouldBe` 0
  it "CShort"   $ (def :: CShort)   `shouldBe` 0
  it "CUShort"  $ (def :: CUShort)  `shouldBe` 0
  it "CInt"     $ (def :: CInt)     `shouldBe` 0
  it "CUInt"    $ (def :: CUInt)    `shouldBe` 0
  it "CLong"    $ (def :: CLong)    `shouldBe` 0
  it "CULong"   $ (def :: CULong)   `shouldBe` 0
  it "CSize"    $ (def :: CSize)    `shouldBe` 0
  it "CWchar"   $ (def :: CWchar)   `shouldBe` 0
  it "CLLong"   $ (def :: CLLong)   `shouldBe` 0
  it "CULLong"  $ (def :: CULLong)  `shouldBe` 0
  it "CFloat"   $ (def :: CFloat)   `shouldBe` 0
  it "CDouble"  $ (def :: CDouble)  `shouldBe` 0

  -- Fixed
  it "Fixed E0" $ (def :: Fixed E0) `shouldBe` 0
  it "Fixed E1" $ (def :: Fixed E1) `shouldBe` 0
  it "Fixed E2" $ (def :: Fixed E2) `shouldBe` 0
  it "Fixed E3" $ (def :: Fixed E3) `shouldBe` 0
  it "Fixed E6" $ (def :: Fixed E6) `shouldBe` 0
  it "Fixed E9" $ (def :: Fixed E9) `shouldBe` 0
  it "Fixed E12"$ (def :: Fixed E12)`shouldBe` 0

  -- System.IO
  it "IOMode"   $ (def :: IOMode)   `shouldBe` ReadMode
  it "Newline"  $ (def :: Newline)  `shouldBe` LF
  it "NewlineMode" $ (def :: NewlineMode) `shouldBe` universalNewlineMode

  -- maybe, either, function, IO
  it "Maybe a"  $ (def :: Maybe Int) `shouldBe` Nothing
  it "Either a b"$(def :: Either Int String) `shouldBe` Left 0
  it "(->) r"   $ (def :: Int -> String) 0 `shouldBe` ""
  it "IO a"     $ (def :: IO Int) `shouldReturn` 0

  -- monoids
  -- could test for mempty, but this is more educational
  it "()"       $ (def :: ())                 `shouldBe` ()
  it "[a]"      $ (def :: [Int])              `shouldBe` []
  it "Ordering" $ (def :: Ordering)           `shouldBe` EQ
  it "Any"      $ (def :: Any)                `shouldBe` Any False
  it "All"      $ (def :: All)                `shouldBe` All True
  it "First a"  $ (def :: First (Maybe Int))  `shouldBe` First Nothing
  it "Last a"   $ (def :: Last (Maybe Int))   `shouldBe` Last Nothing
  it "Sum a"    $ (def :: Sum Float)          `shouldBe` Sum 0
  it "Product a"$ (def :: Product Int)        `shouldBe` Product 1
  it "Endo a"   $ appEndo(def :: Endo Int) 1  `shouldBe` 1
  it "Dual a"   $ getDual (def :: Dual Int)   `shouldBe` 0
  it "()"       $ (def :: (Int,String))       `shouldBe` (0,"")

  -- containers
  it "Set v"    $ (def :: S.Set Int)         `shouldBe` S.empty
  it "Map k v"  $ (def :: M.Map Int Int)     `shouldBe` M.empty
  it "IntMap v" $ (def :: IM.IntMap String)  `shouldBe` IM.empty
  it "IntSet"   $ (def :: IS.IntSet)         `shouldBe` IS.empty
  it "Seq v"    $ (def :: Seq.Seq String)    `shouldBe` Seq.empty
  it "Tree v"   $ (def :: Tree.Tree Int)     `shouldBe` Tree.Node def []

  -- unordered-containers
  it "HashMap k v" $ (def :: HSL.HashMap Int String) `shouldBe` HSL.empty
  it "HashSet v"   $ (def :: HSet.HashSet String)    `shouldBe` HSet.empty

  -- difference lists
  it "DList a"  $ (def :: DList.DList String) `shouldBe` DList.empty

  -- vectors
  it "Vector v"            $ (def :: V.Vector String)  `shouldBe` V.empty
  it "(Storable) Vector v" $ (def :: VS.Vector Char)   `shouldBe` VS.empty
  it "(Unbox) Vector v"    $ (def :: VU.Vector Char)   `shouldBe` VU.empty
  it "(Prim)  Vector v"    $ (def :: VP.Vector Char)   `shouldBe` VP.empty

  -- Text & ByteString
  it "Text"            $ (def :: T.Text)         `shouldBe` ""
  it "Lazy Text"       $ (def :: TL.Text)        `shouldBe` ""
  it "ByteString"      $ (def :: BS.ByteString)  `shouldBe` ""
  it "Lazy ByteString" $ (def :: BSL.ByteString) `shouldBe` ""

  -- Time
  it "Day"             $ (def :: Day)           `shouldBe` ModifiedJulianDay 0
  {-it "UniversalTime"   $ (def :: UniversalTime) `shouldBe` ModJulianDate 0-}
  it "DiffTime"        $ (def :: DiffTime)      `shouldBe` secondsToDiffTime 0
  it "UTCTime"         $ (def :: UTCTime)       `shouldBe` UTCTime def 0

  -- Record
  it "Record" $ (def :: TestRec) `shouldBe` TestRec False "" "" [] (UTCTime def 0)
  it "Recursive Record (left)"  $ (def :: TestList)  `shouldBe` Nil
  it "Recursive Record (right)" $ (def :: TestList2) `shouldBe` Nil2
