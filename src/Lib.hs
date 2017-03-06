{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import           Colors
import           LoadAverage
import           Shell

import           Data.Monoid          ((<>))

import           Text.Printf          (printf)

import           Data.List            (intersperse)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Read       as T

import           Control.Monad        (when)

import           Control.Monad.Except

data BasicBlock = BasicBlock
  { value :: Text
  , fg    :: Color
  , bg    :: Color
  , bold  :: Bool
  }

render :: BasicBlock -> Text
render (BasicBlock val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> code b
                 <> ","
                 <> "fg=" <> code f
                 <> (if bold then ",bold]" else "]")
                 <> v

renderPadL :: BasicBlock -> Text
renderPadL (BasicBlock val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> code b
                 <> ","
                 <> "fg=" <> code f
                 <> (if bold then ",bold]" else "]")
                 <> " " <> v

renderPadR :: BasicBlock -> Text
renderPadR (BasicBlock val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> code b
                 <> ","
                 <> "fg=" <> code f
                 <> (if bold then ",bold]" else "]")
                 <> v <> " "

renderPad :: BasicBlock -> Text
renderPad (BasicBlock val fg bg bold) = go val fg bg bold
  where
    go :: Text -> Color -> Color -> Bool -> Text
    go v f b bold = "#["
                 <> "bg=" <> code b
                 <> ","
                 <> "fg=" <> code f
                 <> (if bold then ",bold]" else "]")
                 <> " " <> v <> " "

simpleSegment cmd fg bg = do
  text <- readShell cmd

  let block = BasicBlock { value = T.take 30 $ T.init text, fg = fg, bg = bg, bold = False }
  liftIO $ T.putStr (renderPad block)

prefixed cmd fg bg pre = do
  text <- readShell cmd

  let block = BasicBlock { value = pre <> T.init text, fg = fg, bg = bg, bold = False }
  liftIO $ T.putStr (renderPad block)

renderAll [] = pure ()
renderAll (x:xs) = mapM_ (liftIO . T.putStr) (renderPad x : (map renderPadR xs))

loadAvg = do
  (LoadAvg last1 last5 last15) <- getLoadAvg
  let template =
        BasicBlock
        {value = undefined, fg = undefined, bg = color "#111111", bold = False}
      blockOf v =
        template
        { value = T.pack $ printf "%.2f" v
        , fg =
            if v > 1.2
              then color "#d54e53"
              else if v > 0.8
                     then color "#e6c547"
                     else color "#b9ca4a"
        }
  renderAll $ map blockOf [last1, last5, last15]

currentSong =
  simpleSegment (cmd "mpc | head -n 1 | sed -r 's/.* - (.*)/\\1/'")
                (color "#dddddd") (color "#111111")

date = do
  simpleSegment (cmd "date +'%a %d %b'")
                (color "#dddddd") (color "#111111")

time = do
  simpleSegment (cmd "date +%H:%M:%S")
                (color "#dddddd") (color "#111111")

backlight = do
  let xbacklightCmd = "xbacklight | cut -d'.' -f1"
  blLevel' <- readShell (cmd xbacklightCmd)
  let (Right blLevel) = fst <$> T.decimal blLevel'
  let fg = color "#7aa6da"
      bg = color "#111111"
      str = T.pack $ show blLevel <> "*"

  let block = BasicBlock { value = str, fg = fg, bg = bg, bold = True }
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

  let block = BasicBlock { value = str, fg = fg, bg = bg, bold = True }
  liftIO $ T.putStr (renderPad block)

separator = liftIO $ T.putStr $ render $
  BasicBlock { value = " "
        , fg = color "#000000"
        , bg = color "#000000"
        , bold = False }

when_ c x = when c (x >> pure ())

-- renderBar :: Bool -> Bool -> IO ()

renderBar = do
  let segments = [currentSong, loadAvg, time, date, backlight, battery]
  -- when_ padBefore $ renderSegment separator
  mapM_ renderSegment (intersperse separator segments)
  -- when_ padAfter $ renderSegment separator
