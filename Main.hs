{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.IO
import           System.Process

data Person = Person
  { name :: {-# UNPACK #-} !T.Text
  , age  :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Generic)

instance ToJSON Person where
  toJSON (Person name age) =
    object [ "name" .= name
           , "age" .= age
           ]

instance FromJSON Person where
  parseJSON = withObject "person" $ \o ->
    Person <$> o .: "name" <*> o .: "age"

main :: IO ()
main = do
  args <- getArgs
  case args of
    "create" : n : _ -> makeFile (read n)
    "parse-copy" : _ -> parseCopy
    "unix-copy" : _  -> unixCopy
    "copy" : _ -> copy
    "copy-bs" : _ -> copyBS
    _ -> do
      putStrLn "create <record-count>: create a json file w/ n records"
      putStrLn "parse-copy: read the created json file, parse it, and write it to a new file"
      putStrLn "copy: read the created json file, and write it to a new file"
      putStrLn "unix-copy: shell out to unix's cp command"
      putStrLn "copy-bs: L.readFile \"file.txt\" >>= L.writeFile \"result.txt\""
      putStrLn "-h: show this help documentation"

-- | 1M records 98% productive, 2MB heap usage, user 0.264s, 24MB
-- | 10M records 98% productive, 2MB heap usage, user 2.67s, 241MB
-- | 100M records 98% productive, 2MB heap usage, user 27.05s, 2.4GB
makeFile :: Int -> IO ()
makeFile n = do
  writeFile "file.txt" mempty
  withFile "file.txt" AppendMode $ \handle ->
    replicateM_ n $ do
      let p = Person { name = "bob", age = 123 }
      L.hPutStr handle (encode p <> "\n")

-- | Helper
validate :: IO ()
validate = do
  exists <- doesFileExist "file.txt"
  when (not exists) (error "call create <count> first")

-- | Parses w/ aeson format
-- 1M
-- real	0m53.169s
-- user	0m52.772s
-- sys	0m0.734s
parseCopy :: IO ()
parseCopy = do
  validate
  writeFile "result.txt" mempty
  lines <- L.lines <$> L.readFile "file.txt"
  withFile "result.txt" AppendMode $ \handle ->
    forM_ lines $ \line -> do
      let Just (p :: Person) = decode line
      L.hPutStr handle (encode p <> "\n")

-- | No parsing done, 95%+ productivity
-- 1M
-- real	0m0.458s
-- user	0m0.385s
-- sys	0m0.073s
--
-- 10M rows
-- real	0m4.495s
-- user	0m3.854s
-- sys	0m0.661s
--
-- 100M rows
-- real	0m45.078s
-- user	0m38.115s
-- sys	0m7.050s
--
copy :: IO ()
copy = do
  validate
  writeFile "result.txt" mempty
  lines <- L.lines <$> L.readFile "file.txt"
  withFile "result.txt" AppendMode $ \handle ->
    forM_ lines $ \line -> do
      L.hPutStr handle (line <> "\n")

-- | No parsing done
-- 1M rows
-- real	0m0.078s
-- user	0m0.010s
-- sys	0m0.046s
--
-- 10M rows
-- real	0m3.367s
-- user	0m2.756s
-- sys	0m0.579s
--
-- 100M rows
-- real	0m11.856s
-- user	0m0.509s
-- sys	0m3.529s
copyBS :: IO ()
copyBS = do
  validate
  writeFile "result.txt" mempty
  L.readFile "file.txt" >>= L.writeFile "result.txt"

-- | No parsing done, just copying using cp, assume high productivity
-- 1M
-- real	0m0.120s
-- user	0m0.008s
--sys	0m0.049s
--
-- 10M rows
-- real	0m0.601s
-- user	0m0.011s
-- sys	0m0.205s
--
-- 100M rows
-- real	0m9.682s
-- user	0m0.014s
-- sys	0m2.673s
unixCopy :: IO ()
unixCopy = do
  validate
  writeFile "result.txt" mempty
  void $ system "cp file.txt result.txt"
