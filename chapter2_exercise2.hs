

substituteChar :: Char -> Char
substituteChar c =
    case c of
        'e' -> '3'
        'a' -> '4'
        'o' -> '0'
        't' -> '7'
        _   -> c


translateWord :: String -> String
translateWord xs = map substituteChar xs


main :: IO ()
main = 
    do 
        putStr "Please enter a word.\n> "
        word <- getLine
        print (translateWord word  )    