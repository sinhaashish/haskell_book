import Data.Char

newtype Password = Password String
    deriving Show

newtype Username = Username String
    deriving Show

newtype Error = Error String
    deriving Show

data User = User Username Password
    deriving Show


checkUsernameLength :: String -> Either Error Username
checkUsernameLength username =
    case (length username > 8) of
        True -> Left ( Error " Username can't be more than 8 characters.")
        False -> Right (Username username)

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
    case (length password > 20) of
        True -> Left ( Error " Password can't be more than 20 characters.")
        False -> Right (Password password)

cleanWhiteSpace :: String -> Either Error String
cleanWhiteSpace "" = Left ( Error " It can not be empty")
cleanWhiteSpace (x:xs) =
    case (isSpace x) of 
        True -> cleanWhiteSpace xs
        False -> Right (x:xs)

 

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
    case ( all isAlphaNum xs) of
        True -> Right xs
        False -> Left (Error "can contain alpha numeric character only ")


validateUserName :: Username -> Either Error Username
validateUserName (Username username )=
    cleanWhiteSpace username
    >>= requireAlphaNum
    >>= checkUsernameLength

validatePassword :: Password -> Either Error Password
validatePassword (Password password) = 
    cleanWhiteSpace password 
    >>= requireAlphaNum
        >>= checkPasswordLength  
          
makeUser :: Username -> Password -> Either Error User
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
