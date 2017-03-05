{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import           Colors

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import           Data.Monoid          ((<>))

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Read       as T

import           Control.Monad.Except

import           GHC.IO.Exception     (ExitCode, ExitCode (..))
import qualified System.Process.Typed as P



data FormattedBlock = Block
  { value :: Text
  , fg    :: Color
  , bg    :: Color
  , bold  :: Bool
  }

render :: FormattedBlock -> Text
render (Block val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> code b
                 <> ","
                 <> "fg=" <> code f
                 <> (if bold then ",bold]" else "]")
                 <> v

renderPad :: FormattedBlock -> Text
renderPad (Block val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> code b
                 <> ","
                 <> "fg=" <> code f
                 <> (if bold then ",bold]" else "]")
                 <> " " <> v <> " "

newtype ErrorCode = ErrorCode Int
newtype ShellCmd  = ShellCmd Text
newtype Result a = Result
  { unR :: ExceptT ErrorCode IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorCode
             , MonadIO
             )

cmd = ShellCmd

readShell :: ShellCmd -> Result Text
readShell (ShellCmd cmd) = do
  (err, out, errText) <- liftIO . P.readProcess . P.shell $ T.unpack cmd
  -- case err of
    -- ExitSuccess      -> pure (T.decodeUtf8 $ BL.toStrict out)
  pure (T.decodeUtf8 $ BL.toStrict out)
    -- ExitFailure code -> do
    --   liftIO $ putStrLn "Error!"
    --   throwError (ErrorCode code)

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

time = do
  let dateCmd = "date +%H:%M:%S"
  time <- readShell (cmd dateCmd)
  let fg = color "#dddddd"
      bg = color "#333333"
      str = T.init time

  let block = Block { value = str, fg = fg, bg = bg, bold = False }
  liftIO $ T.putStr (renderPad block)

backlight = do
  let xbacklightCmd = "xbacklight | cut -d'.' -f1"
  blLevel' <- readShell (cmd xbacklightCmd)
  let (Right blLevel) = fst <$> T.decimal blLevel'
  let fg = color "#7aa6da"
      bg = color "#333333"
      str = T.pack $ show blLevel <> "*"

  let block = Block { value = str, fg = fg, bg = bg, bold = True }
  liftIO $ T.putStr (renderPad block)

battery = do
  let
    percRegex = "s/.*, ([0-9]*)%.*/\\1/"
    percCmd   = "acpi -b | sed -r '" <> percRegex <> "'"
    statusCmd = "acpi -b | grep -c 'Dis'"

  perc' <- readShell (cmd percCmd)
  let (Right perc) = fst <$> T.decimal perc'

  stat' <- readShell (cmd statusCmd)
  let (Right charging)  = (==0) . fst <$> T.decimal stat'

  let str = case perc of
        100       -> "100"
        otherwise -> (T.pack $ show perc)
                  <> (if charging then "+" else "-")

  let fg = if charging then color "#b9ca4a" else color "#d54e53"
      bg = color "#111111"

  let block = Block { value = str, fg = fg, bg = bg, bold = True }
  liftIO $ T.putStr (renderPad block)

separator = liftIO $ T.putStr $ render $
  Block { value = " "
        , fg = color "#000000"
        , bg = color "#000000"
        , bold = False }

runR = runExceptT . unR

renderBar :: IO ()
renderBar = do
  forM_ [time, backlight, battery] $ \segment -> do
    runR separator
    runR segment
