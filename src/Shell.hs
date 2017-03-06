{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shell where

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

newtype ErrorCode = ErrorCode Int
newtype ShellCmd  = ShellCmd Text
newtype RemuxM a = RemuxM
  { unR :: ExceptT ErrorCode IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorCode
             , MonadIO
             )

cmd = ShellCmd

readShell :: ShellCmd -> RemuxM Text
readShell (ShellCmd cmd) = do
  (err, out, errText) <- liftIO . P.readProcess . P.shell $ T.unpack cmd
  -- case err of
    -- ExitSuccess      -> pure (T.decodeUtf8 $ BL.toStrict out)
  pure (T.decodeUtf8 $ BL.toStrict out)
    -- ExitFailure code -> do
    --   liftIO $ putStrLn "Error!"
    --   throwError (ErrorCode code)

renderSegment = runExceptT . unR
