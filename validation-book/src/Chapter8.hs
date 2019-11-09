module Main where
import Data.Char
import Data.Validation

newtype Password = Password String
    deriving Show

newtype Username = Username String
    deriving Show

newtype Error = Error [String]
    deriving Show

instance Semigroup Error where
    Error xs <> Error ys = Error (xs <> ys)

data User = User Username Password
    deriving Show


checkUsernameLength :: String -> Validation Error Username
checkUsernameLength username =
    case (length username > 8) of
        True -> Failure (Error [" Username can't be more than 8 characters."] )
        False -> Success (Username username)

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
    case (length password > 20) of
        True -> Failure (Error [" Password can't be more than 20 characters."] )
        False -> Success (Password password)

cleanWhiteSpace :: String -> Validation Error String
cleanWhiteSpace "" = Failure ( Error [" It can not be empty"])
cleanWhiteSpace (x:xs) =
    case (isSpace x) of 
        True -> cleanWhiteSpace xs
        False -> Success (x:xs)
 
requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
    case (all isAlphaNum xs) of
        False -> Failure (Error ["Cannot contain white space or special characters."] )
        True -> Success xs


validateUserName :: Username -> Validation Error Username
validateUserName (Username username )=
    cleanWhiteSpace username
    >>= requireAlphaNum
    >>= checkUsernameLength

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) = 
    cleanWhiteSpace password 
    >>= requireAlphaNum
        >>= checkPasswordLength  
          
makeUser :: Username -> Password -> Validation Error User
makeUser name password =
    User <$> validateUserName name
    <*> validatePassword password



main :: IO ()
main =
    do
-- getLine has the type IO String;
-- password has the type String.
-- <$> apply the Password constructor inside the IO, changing our IO String
-- into an IO Password which allows the Password to be passed to validate-
-- Password (and allows us to go on ignoring IO).
        putStr "Please enter a Username.\n> "
        username <- Username <$> getLine
        putStr "Please enter a password.\n> "
        password <- Password <$> getLine
        -- print (validateUserName username)
        -- print (validatePassword password)
        print (makeUser username password)