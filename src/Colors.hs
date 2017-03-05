{-# LANGUAGE OverloadedStrings #-}

module Colors where

import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as T

newtype Color = Color { code :: Text }

color :: Text -> Color
color = Color
