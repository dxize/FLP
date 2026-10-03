{-# OPTIONS_GHC -Wno-x-partial #-}

main = do
    print (head ( tail ['a', 'b', 'c']))
    print (head (tail ( head [['a', 'b'], ['c','d']])))
    print (head (tail (head (tail [['a', 'c', 'd'], ['a','b']]))))
    print (head (head (tail [['a','d'], ['b', 'c']])))