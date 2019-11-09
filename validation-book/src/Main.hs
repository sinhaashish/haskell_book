module Main where
import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password < 8 || length password > 20) of 
    False -> Just password
    True -> Nothing

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs = 
  case (all isAlphaNum xs) of 
    True -> Just xs
    False -> Nothing

cleanWhiteSpace :: String -> Maybe String
cleanWhiteSpace "" = Nothing
cleanWhiteSpace ( x: xs) =
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Just (x:xs)

validatePassword :: String -> Maybe String
validatePassword password = 
  case ( cleanWhiteSpace password ) of
    Nothing -> Nothing
    Just password2 ->
      case ( requireAlphaNum password2 )  of
        Nothing -> Nothing
        Just password3 ->
          case ( checkPasswordLength password3 ) of 
            Nothing -> Nothing
            Just password4 -> Just password4       

 -- Using Bind and Either
checkPasswordLengthEither :: String -> Either String String
checkPasswordLengthEither password =
  case (length password < 8 || length password > 20) of 
    False -> Right password  
    True -> Left "Password length must be between 10 to 20 character "
    
requireAlphaNumEither :: String -> Either String String
requireAlphaNumEither xs = 
  case (all isAlphaNum xs) of 
    True -> Right xs
    False -> Left "Password can contain alpha numeric character only"     

cleanWhiteSpaceEither :: String -> Either String String
cleanWhiteSpaceEither "" = Left "Password can not be be empty"
cleanWhiteSpaceEither ( x: xs) =
  case (isSpace x) of
    True -> cleanWhiteSpaceEither xs
    False -> Right (x:xs)    

validatePasswordEither :: String -> Either String String
validatePasswordEither password = 
  cleanWhiteSpaceEither password 
    >>= checkPasswordLengthEither
      >>= requireAlphaNumEither      





-- Using Bind 
validatePassword1 :: String -> Maybe String
validatePassword1 password = 
  cleanWhiteSpace password 
    >>= requireAlphaNum 
      >>= checkPasswordLength

validatePassword2 :: String -> String
validatePassword2 password = 
  case ( cleanWhiteSpace password ) of
    Nothing -> " Password can not be empty"
    Just password2 ->
      case ( requireAlphaNum password2 )  of
        Nothing -> " Password must be alpha numeric only"
        Just password3 ->
          case ( checkPasswordLength password3 ) of 
            Nothing -> " Password length must be between 10 to 20 character"
            Just password4 -> password4  
  
-- New Type
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
    case (length username > 6) of 
      False -> Right (Username username)  
      True -> Left ( Error "Password length must be between 10 to 20 character ") 

checkLength :: Int -> String -> Either Error String
checkLength int str =
  case (length str > int) of
    True -> Left (Error "Input exceeded maximum allowed length.")
    False -> Right str      
    
requireAlphaNumNewType :: String -> Either Error String
requireAlphaNumNewType xs = 
    case (all isAlphaNum xs) of 
      True -> Right xs
      False -> Left (Error "Password can contain alpha numeric character only"  )   

cleanWhiteSpaceNewType :: String -> Either Error String
cleanWhiteSpaceNewType "" = Left (Error "Password can not be be empty")
cleanWhiteSpaceNewType ( x: xs) =
    case (isSpace x) of
      True -> cleanWhiteSpaceNewType xs
      False -> Right (x:xs)    

validatePasswordNewType :: Password -> Either Error Password
validatePasswordNewType (Password password) = 
  cleanWhiteSpaceNewType password 
    >>= requireAlphaNumNewType
      >>= checkPasswordLengthNewType   

validatePasswordGeneric :: Password -> Either Error Password
validatePasswordGeneric (Password password) = 
  Password <$> 
  (cleanWhiteSpaceNewType password 
  >>= requireAlphaNumNewType
  >>= checkLength 20)      

validateUsernameNewType :: Username -> Either Error Username
validateUsernameNewType (Username username) = 
  cleanWhiteSpaceNewType username 
    >>= requireAlphaNumNewType
      >>= checkUsernameLengthNewType   
      

-- main1 :: IO ()
-- main1 =
--   putStrLn "Please enter a password\n> " >>
--   (Password <$> getLine) >>= \ 
--   \pwd -> print (validatePassword pwd)

          

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "Please enter the password"
-- getLine has the type IO String;
-- password has the type String.
-- <$> apply the Password constructor inside the IO, changing our IO String
-- into an IO Password which allows the Password to be passed to validate-
-- Password (and allows us to go on ignoring IO).
  password <- Password <$> getLine
  print( validatePasswordNewType password)

----- Chapter 3 Exercise 10
reverseLine :: IO ()
reverseLine = getLine >>= (print . reverse)

reverseLine1 :: IO ()
reverseLine1 = do
  putStrLn "Please enter the password"
  password <- getLine
  print( reverse password)

----- Chapter 3 Exercise 11
bindMaybe Nothing func = Nothing
bindMaybe (Just x) func = func x


----- Chapter 3 Exercise 12
data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a-> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str xs) func = Str xs
bindStringOrValue (Val x) func = func x




printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of 
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."
    
eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines 
      [ "Test " ++ show n
      , " Expected: " ++ show expected
      , " But got: " ++ show actual
      ])

data Pair a = Pair a a

test :: IO ()
test = printTestResult $
  do
    -- checkPasswordLengthEither
    eq 1 (checkPasswordLengthEither "") (Left  "Password length must be between 10 to 20 character ")
    eq 2 (checkPasswordLengthEither "julielovesbooks") (Right "julielovesbooks")
    eq 3 (checkPasswordLengthEither "julielovesbooks12345678") (Left  "Password length must be between 10 to 20 character ") 
    eq 4 (checkPasswordLengthEither "julieloves        books") (Left  "Password length must be between 10 to 20 character ")
    eq 5 (checkPasswordLengthEither " ") (Left  "Password length must be between 10 to 20 character ")

    -- requireAlphaNumEither
    eq 6 (requireAlphaNumEither "julielovesbooks") (Right "julielovesbooks") 
    eq 7 (requireAlphaNumEither "julielovesb ooks") (Left "Password can contain alpha numeric character only") 
    eq 8 (requireAlphaNumEither "julielovesb(ooks") (Left "Password can contain alpha numeric character only")  
    eq 9 (requireAlphaNumEither "julielovesb_ooks") (Left "Password can contain alpha numeric character only")  
    eq 10 (requireAlphaNumEither "julielovesb-ooks") (Left "Password can contain alpha numeric character only") 

    -- cleanWhiteSpaceEither
    eq 11 (cleanWhiteSpaceEither "julielovesbooks") (Right "julielovesbooks") 
    eq 12 (cleanWhiteSpaceEither "    julielovesbooks") (Right "julielovesbooks") 
    eq 13 (cleanWhiteSpaceEither "julielovesbooks ") (Right "julielovesbooks ") 
    eq 14 (cleanWhiteSpaceEither "") (Left "Password can not be be empty") 
 
