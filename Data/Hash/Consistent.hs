{- | Data.Hash.Consistent

A consistent hash is a technique to manage the fair distribution of cacheable
entities among hosts. Each host identifier has its crc32 hash calculated
and stored in a Vector along with its canonical host name. The host identifier
may be differentiated from its canonical host name by a multiplying factor,
in our case a simple integer appeneded to the hostname to provide it with
a number of entries in the consistent hash, all evenly distributed. 

This technique is explained in these links:

http://en.wikipedia.org/wiki/Consistent_hashing

http://www.tomkleinpeter.com/2008/03/17/programmers-toolbox-part-3-consistent-hashing/

Here is a small program illustrating its use:

@

  module Main where
  import Data.Hash.Consistent as CH

  main = do
    let hosts  = ["hi.example.net","bar.example.net","foo.example.net"] :: [CH.Host]
    let n = 2 :: Int
    let ch = new
    print $ show $ ch
    ch <- return $ add hosts n ch
    print $ show $ ch  
    let fh = [head hosts] :: [Host]
    let hh_fh = hash_hosts fh n
    print hh_fh
    ch <- return $ del fh n ch
    print $ show $ ch  
    let i = 770931073
    let tgt = target_host i ch 
    print tgt
    return ()
@

License info:

The license is a simple BSD3-style license available here:
https://www.b7j0c.org/stuff/license.txt

-}

module Data.Hash.Consistent (text_crc32,search,add,del,target_host,hash_hosts,
                             Host,Hash,HashHost,ConsistentHash) where

import qualified Control.Monad.ST              as ST
import qualified Data.Word                     as W
import qualified Data.Ord                      as O
import qualified Data.ByteString.UTF8          as BU
import qualified Data.Digest.CRC32             as CRC
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as I
import qualified Data.Vector.Algorithms.Search as S

type Host            = String
type Hash            = W.Word32
type HashHost        = (Hash,Host)
type ConsistentHash  = V.Vector HashHost 

-- a convenience method to generate a crc for a string
text_crc32 :: String -> Hash
text_crc32 = CRC.crc32 . BU.fromString

-- sort the consistent hash by the hash values
sort :: ConsistentHash -> ConsistentHash
sort v = ST.runST $ do
  m <- V.unsafeThaw v
  I.sortBy (O.comparing fst) m
  v' <- V.unsafeFreeze m
  return v'

-- find the index of the first host hash greater than i
search :: Hash -> ConsistentHash -> Int
search i v = 
  ST.runST $ do 
    m <- V.unsafeThaw v
    idx <- S.binarySearchBy (O.comparing fst) m (i,"")
    return idx

-- locate which host should cache the item with key hash i
target_host :: Hash -> ConsistentHash -> HashHost
target_host i ch = let idx = search i ch 
                       l   = V.length ch in
                   if idx >= l then ch V.! 0 else ch V.! idx

new = V.empty :: ConsistentHash

-- for a set of servers and a multiplying factor, return a list of
-- crc hash values
hash_hosts :: [Host] -> Int -> ConsistentHash
hash_hosts hosts mult = 
  V.fromList [(text_crc32 $ x++y,x)|x<-hosts,y<-map show [1..mult]] 

-- add a list of hosts to the consistent hash
add :: [Host] -> Int -> ConsistentHash -> ConsistentHash
add hosts mult ch = sort $ ch V.++ (hash_hosts hosts mult) 

-- delete a list of hosts from the consistent hash
del :: [Host] -> Int -> ConsistentHash -> ConsistentHash
del hosts mult ch = let hh = hash_hosts hosts mult in
  sort $ V.filter (\a -> V.notElem a hh) (ch) 

  
