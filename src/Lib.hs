{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Colors
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           GHC.IO.Exception     (ExitCode, ExitCode (..))
import qualified System.Process.Typed as P

{-
set -g status-position bottom
set -g status-bg black
set -g status-fg colour137
set -g status-attr dim
set -g status-left ""
set -g status-right "#[fg=colour250,bg=colour235] #(cat /proc/loadavg | cut -d' ' -f1-3) #[bg=colour234] #[fg=colour250,bg=colour235] %a %d %b #[bg=colour234] #[fg=colour250,bg=colour235] %H:%M:%S #[bg=colour234] #[fg=colour81,bg=colour238,bold] #(xbacklight | cut -d'.' -f1)% #[bg=colour234] #[fg=colour81,bg=colour238,bold] #(acpi -b | grep -o '[0-9]*%') "
set -g status-right "#[fg=colour250,bg=colour235] #(mpc | head -n 1 | sed -r 's/.* - (.*)/\\1/') #[bg=colour234] #[fg=colour250,bg=colour235] %a %d %b #[bg=colour234] #[fg=colour250,bg=colour235] %H:%M:%S #[bg=colour234] #[fg=colour81,bg=colour238,bold] #(xbacklight | cut -d'.' -f1)% #[bg=colour234] #[fg=colour81,bg=colour238,bold] #(acpi -b | grep -o '[0-9]*%') "
set -g status-right-length 100
set -g status-left-length 20
-}


data AlertSegment = Alert
  { value :: Text
  , fg    :: Color
  , bg    :: Color
  , bold  :: Bool
  }

render :: AlertSegment -> Text
render (Alert val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> renderColor b
                 <> ","
                 <> "fg=" <> renderColor f
                 <> (if bold then ",bold]" else "]")
                 <> v

newtype ErrorCode = ErrorCode Int
newtype ShellCmd  = ShellCmd Text
type CmdResult = Either ErrorCode Text
type Result a = IO (Either ErrorCode a)

readShell :: ShellCmd -> IO CmdResult
readShell (ShellCmd cmd) = do
  (err, out, errText) <- P.readProcess (P.shell $ T.unpack cmd)
  pure $ -- trying hard to not bimap here, or even either
    case err of
      ExitSuccess      -> Right (T.decodeUtf8 $ BL.toStrict out)
      ExitFailure code -> Left (ErrorCode code)

createSegment :: ShellCmd
              -> (Text -> AlertSegment)
              -> Result AlertSegment
createSegment cmd go =
  (fmap . fmap) go (readShell cmd)

statusLine :: Result AlertSegment -> Result Text
statusLine = (fmap . fmap) render

someFunc :: IO ()
someFunc = putStrLn "someFunc"
