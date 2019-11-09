reverseLine :: IO ()
reverseLine = getLine >>= (print . reverse)

main :: IO ()
main = do
  putStrLn "Please enter the password"
  password <- getLine
  print( reverse password)