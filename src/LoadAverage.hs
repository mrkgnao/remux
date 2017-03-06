{-# LANGUAGE OverloadedStrings #-}

module LoadAverage where

import           Control.Monad.Except
import           Data.Attoparsec.Text
import           Shell

data LoadAvg = LoadAvg
            { last1  :: Double
            , last5  :: Double
            , last15 :: Double
            } deriving (Show, Eq)

getLoadAvg :: RemuxM LoadAvg
getLoadAvg = do
  text <- readShell (cmd "cat /proc/loadavg")
  -- let loadAvg = do
  --       last1 <- double
  --       last5 <- double
  --       last15 <- double
  --       pure $ LoadAvg last1 last5 last15
  let loadAvg = LoadAvg
             <$> double <* space
             <*> double <* space
             <*> double
  let (Right res) = parseOnly loadAvg text
  -- liftIO $ print res
  pure res
