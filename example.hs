module Main where
import qualified Data.Hash.Consistent as CH

main = do
    let hosts  = ["hi.example.net","bar.example.net","foo.example.net"] :: [CH.Host]
    let n = 2 :: Int
    let ch = CH.new
    ch <- return $ CH.add hosts n ch
    print $ show $ ch  
    let fh = [head hosts] :: [CH.Host]
    let hh_fh = CH.hashHosts fh n
    print hh_fh
    ch <- return $ CH.del fh n ch
    print $ show $ ch  
    let i = 770931073
    let tgt = CH.targetHost i ch 
    print i
    print tgt
    return ()
