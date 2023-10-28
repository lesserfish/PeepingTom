module Commands.Base (Command (..), getString) where

import State

data Command
    = ACommand
        { cName :: String
        , action :: [String] -> State -> IO State
        , help :: String
        }
    | HCommand
        { cName :: String
        , subcommands :: [Command]
        , help :: String
        }

instance Show Command where
    show (ACommand name _ _) = name
    show (HCommand name sub _) = name ++ " > " ++ (show sub)

getStringHelper :: String -> String
getStringHelper ('"' : rest) = reverse (go $ reverse rest)
  where
    go :: String -> String
    go ('"' : rest) = rest
    go str = str
getStringHelper ('\'' : rest) = reverse (go $ reverse rest)
  where
    go :: String -> String
    go ('\'' : rest) = rest
    go str = str
getStringHelper str = str

getString :: [String] -> Maybe String
getString [] = Just $ ""
getString (y : ys)
    | fchr == '"' = Just $ getStringHelper (unwords (y : ys))
    | fchr == '\'' = Just $ getStringHelper (unwords (y : ys))
    | length ys > 0 = Nothing
    | otherwise = Just $ y
  where
    fchr = if length y == 0 then '\0' else y !! 0
