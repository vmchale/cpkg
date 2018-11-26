module Package.C
    ( exec
    , head'
    ) where

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

exec :: IO ()
exec = putStrLn "cpkg from template"