{-

Copyright (c) 2010, 2012 Lukas Mai
              2013, 2014 Jonathan Fischoff, João Cristóvão

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the author nor the names of his contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY LUKAS MAI AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Default.Generics (
-- | This module defines a class for types with a default value. Instances are
-- provided for '()', 'S.Set', 'M.Map', 'Int', 'Integer', 'Float', 'Double',
-- and many others (see below).
    Default(..), genericDefault
) where

import Data.Int
import Data.Word
import Data.Fixed
import Foreign.C.Types
import Data.Monoid
import Data.Ratio
import Data.Complex
import qualified Data.Set as S
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Data.Tree (Tree(..))
import Data.DList (DList)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy    as HSL
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import qualified Data.Vector.Primitive as VP
-- Strict only in functions, data structure is the same
{-import qualified Data.HashMap.Strict  as HSS-}
import qualified Data.HashSet         as HSet
import System.Locale
import System.IO
import GHC.Generics

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- | Generic default value. Useful when creating your own instance of Default
-- with only few values different.
genericDefault :: (Generic a, GDefault (Rep a)) => a
genericDefault = to gDef

-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a
    default def :: (Generic a, GDefault (Rep a)) => a
    def = genericDefault

instance Default Bool where def = False

instance Default Int where def = 0
instance Default Int8 where def = 0
instance Default Int16 where def = 0
instance Default Int32 where def = 0
instance Default Int64 where def = 0
instance Default Word where def = 0
instance Default Word8 where def = 0
instance Default Word16 where def = 0
instance Default Word32 where def = 0
instance Default Word64 where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0
instance (Integral a) => Default (Ratio a) where def = 0
instance (Default a, RealFloat a) => Default (Complex a) where def = def :+ def

-- C Interface
instance Default CChar    where def = 0
instance Default CSChar   where def = 0
instance Default CUChar   where def = 0
instance Default CShort   where def = 0
instance Default CUShort  where def = 0
instance Default CInt     where def = 0
instance Default CUInt    where def = 0
instance Default CLong    where def = 0
instance Default CULong   where def = 0
-- instance Default CPtrdiff where def = 0 -- this just seems wrong
instance Default CSize    where def = 0
instance Default CWchar   where def = 0
instance Default CLLong   where def = 0
instance Default CULLong  where def = 0
instance Default CFloat   where def = 0
instance Default CDouble  where def = 0

instance Default (Fixed E0) where def = 0
instance Default (Fixed E1) where def = 0
instance Default (Fixed E2) where def = 0
instance Default (Fixed E3) where def = 0
instance Default (Fixed E6) where def = 0
instance Default (Fixed E9) where def = 0
instance Default (Fixed E12)where def = 0


-- System.IO
-- should these be here?
instance Default IOMode       where def = ReadMode
instance Default TextEncoding where def = utf8
instance Default Newline      where def = LF
instance Default NewlineMode  where def = universalNewlineMode

-- maybe, either, function, IO
instance Default (Maybe a) where def = Nothing
instance (Default a) => Default (Either a b)  where def = Left def
instance (Default r) => Default (e -> r)      where def = const def
instance (Default a) => Default (IO a)        where def = return def

-- monoids
instance Default () where def = mempty
instance Default [a] where def = mempty
instance Default Ordering where def = mempty
instance Default Any where def = mempty
instance Default All where def = mempty
instance Default (First a) where def = mempty
instance Default (Last a) where def = mempty
instance (Num a) => Default (Sum a) where def = mempty
instance (Num a) => Default (Product a) where def = mempty
instance Default (Endo a) where def = mempty
instance (Default a) => Default (Dual a) where def = Dual def
instance (Default a, Default b) => Default (a, b) where def = (def, def)
instance (Default a, Default b, Default c) => Default (a, b, c) where def = (def, def, def)
instance (Default a, Default b, Default c, Default d) => Default (a, b, c, d) where def = (def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e) => Default (a, b, c, d, e) where def = (def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f) => Default (a, b, c, d, e, f) where def = (def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g) => Default (a, b, c, d, e, f, g) where def = (def, def, def, def, def, def, def)

-- containers
instance Default (S.Set v)    where def = S.empty
instance Default (M.Map k v)  where def = M.empty
instance Default (IntMap v)   where def = mempty
instance Default IntSet       where def = mempty
instance Default (Seq a)      where def = mempty
instance (Default a) => Default (Tree a) where def = Node def def

-- unordered-containers
instance Default (HSL.HashMap k v) where def = HSL.empty
instance Default (HSet.HashSet v)  where def = HSet.empty

-- difference lists
instance Default (DList a)    where def = mempty

-- vectors
instance Default (V.Vector v)        where def = V.empty
instance (VS.Storable v) => Default (VS.Vector v) where def = VS.empty
instance (VU.Unbox    v) => Default (VU.Vector v) where def = VU.empty
instance (VP.Prim     v) => Default (VP.Vector v) where def = VP.empty


instance Default T.Text   where def = T.empty
instance Default TL.Text  where def = TL.empty
instance Default BS.ByteString  where def = BS.empty
instance Default BSL.ByteString where def = BSL.empty




instance Default TimeLocale where def = defaultTimeLocale

-- Data.Time.Calendar
instance Default Day             where def = ModifiedJulianDay 0
-- Data.Time.Clock
instance Default UniversalTime   where def = ModJulianDate 0
instance Default DiffTime        where def = secondsToDiffTime 0
instance Default UTCTime         where def = UTCTime def def
instance Default NominalDiffTime where def = diffUTCTime def def
-- Data.Time.LocalTime
instance Default TimeZone        where def = utc
instance Default TimeOfDay       where def = midnight
instance Default LocalTime       where def = utcToLocalTime utc def
instance Default ZonedTime       where def = utcToZonedTime utc def

-- Generic Default for default implementation
class GDefault f where
    gDef :: f a

instance GDefault U1 where
    gDef = U1

instance (Datatype d, GDefault a) => GDefault (D1 d a) where
   gDef = M1 gDef

instance (Constructor c, GDefault a) => GDefault (C1 c a) where
   gDef = M1 gDef

instance (Selector s, GDefault a) => GDefault (S1 s a) where
   gDef = M1 gDef

instance (Default a) => GDefault (K1 i a) where
   gDef = K1 def

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
   gDef = gDef :*: gDef

instance (HasRec a, GDefault a, GDefault b) => GDefault (a :+: b) where
   gDef = if hasRec' (gDef :: a p) then R1 gDef else L1 gDef

--------------------------------------------------------------------------------
-- | We use 'HasRec' to check for recursion in the structure. This is used
-- to avoid selecting a recursive branch in the sum case for 'Empty'.
class HasRec a where
  hasRec' :: a x -> Bool
  hasRec' _ = False

instance HasRec V1
instance HasRec U1

instance (HasRec a) => HasRec (M1 i j a) where
  hasRec' (M1 x) = (hasRec' x)

instance (HasRec a, HasRec b)
  => HasRec (a :+: b) where
  hasRec' (L1 x) = hasRec' x
  hasRec' (R1 x) = hasRec' x

instance (HasRec a, HasRec b)
  => HasRec (a :*: b) where
  hasRec' (a :*: b) = hasRec' a || hasRec' b

instance HasRec (Rec0 b) where
  hasRec' (K1 _) = True

