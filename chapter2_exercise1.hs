import Data.Char (isAlpha, toLower)
import Data.List

isWord word =
    case (filter isAlpha word) of
    [] -> Nothing
    word -> Just (map toLower word)

isPalindrome :: String -> Bool
isPalindrome word = ( word  == reverse word)

checkPalindrome :: String -> String
checkPalindrome word =
    case (isWord word) of 
        Nothing -> "The word is invalid" 
        Just word -> 
            case ( isPalindrome word) of 
                True -> " Words are palindrome"
                False -> " Words are not palindrome"

main :: IO ()
main =
    do 
        putStr "Please enter a word.\n> "
        word <- getLine
        print(checkPalindrome(word) )              

