module Model where

alexScanTokens :: String -> [Token]

-- Exercise 1
data Token =
{
    tokenType :: String,
    tokenValue :: String
}
Token deriving Show


-- Exercise 2
data Program = Program deriving Show
