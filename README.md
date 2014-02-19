data-default-generics
=====================

A class for types with a default value.

This is a fork of the package originally developed by Lukas Mai, [data-default](http://hackage.haskell.org/package/data-default), 
with the following additions:

* Regrouped everything in a [single module](https://github.com/mauke/data-default/commit/312844532f3c3c79fc5b0e911bffa74baf24b2f7#commitcomment-3989849)
* Added several instances, namely Either, Text, ByteString, etc
* Added Generics support by [Jonathan Fischoff](https://github.com/mauke/data-default/pull/4), 
  with some additional support for recursive datatypes by me, with the precious help of [José Pedro Magalhães](http://dreixel.net/)
  
It should be usable as a drop-in replacement.  

Usage
=====

Supose you've got a structure:

```
data TestRec = TestRec
  { bool    :: Bool
  , txt     :: T.Text
  , bs      :: BSL.ByteString
  , lst     :: [CInt]
  , ut      :: UTCTime
  } deriving (Eq,Show)
```

And you want to quickly define a default value for it, in a way that does not require code changes if the structure later gets altered.
Modify the previous to:


```
{-# LANGUAGE DeriveGeneric       #-} 

(...)
import Data.Default.Generics
import GHC.Generics
(...)

data TestRec = TestRec
  { bool    :: Bool
  , txt     :: T.Text
  , bs      :: BSL.ByteString
  , lst     :: [CInt]
  , ut      :: UTCTime
  } deriving (Eq,Show, Generic)
  
instance Default TestRec 
```

And then you can use the default value for this structure as ```def```, as long as you import ```Data.Default.Generics```.

Bugs 
=====

Issues and pull requests are most welcome. Any additional instance for data types from the Haskell Platform will be considered.
