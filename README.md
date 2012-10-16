haskell-Data.Hash.Consistent
============================

## Data.Hash.Consistent

A consistent hash is a technique to manage the fair distribution of cacheable
entities among hosts. Each host identifier has its crc32 hash calculated
and stored in a Vector along with its canonical host name. The host identifier
may be differentiated from its canonical host name by a multiplying factor,
in our case a simple integer appeneded to the hostname to provide it with
a number of entries in the consistent hash, all evenly distributed. 

This technique is explained in these links:

http://en.wikipedia.org/wiki/Consistent_hashing

http://www.tomkleinpeter.com/2008/03/17/programmers-toolbox-part-3-consistent-hashing/
