import           Parser

main :: IO ()
main = do
    file <- readFile "example/bool.txt"
    case regularParse expr file of
        Left  err -> error (show err)
        Right out -> print out
