import System.IO

import Scanner

main = do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    print (scan line)
    main
