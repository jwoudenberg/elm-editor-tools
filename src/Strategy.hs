module Strategy
  ( (<|>)
  , choice
  , Strategy
  ) where

type Strategy a = IO (Maybe a)

(<|>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
(<|>) first next = first >>= result
  where
    result Nothing = next
    result (Just value) = pure (Just value)

choice :: [IO (Maybe a)] -> IO (Maybe a)
choice xs = foldl (<|>) (pure Nothing) xs
