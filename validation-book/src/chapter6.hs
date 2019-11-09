newtype Password = Password String
    deriving Show

newtype Username = Username String
    deriving Show

newtype Error = Error String
    deriving Show

checkPasswordLengthNewType :: String -> Either Error Password
checkPasswordLengthNewType password =
    case (length password > 20) of 
    False -> Right (Password password)  
    True -> Left ( Error "Password length must be between 10 to 20 character ") 

checkUsernameLengthNewType :: String -> Either Error Username
checkUsernameLengthNewType username =
    case (length password > 6) of 
    False -> Right (Username password)  
    True -> Left ( Error "Password length must be between 10 to 20 character ") 
    
requireAlphaNumNewType :: String -> Either Error String
requireAlphaNumNewType xs = 
    case (all isAlphaNum xs) of 
    True -> Right xs
    False -> Left (Error "Password can contain alpha numeric character only"  )   

cleanWhiteSpaceNewType :: String -> Either Error String
cleanWhiteSpaceNewType "" = Left (Error "Password can not be be empty")
cleanWhiteSpaceNewType ( x: xs) =
    case (isSpace x) of
    True -> cleanWhiteSpaceEither xs
    False -> Right (x:xs)    

validatePasswordNewType :: String -> Either String String
validatePasswordNewType (Password password) = 
    cleanWhiteSpace password 
    >>= checkPasswordLength
        >>= requireAlphaNum   