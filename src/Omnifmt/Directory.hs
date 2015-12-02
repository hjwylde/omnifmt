{-|
Module      : Omnifmt.Directory
Description : System directory utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

System directory utilities.
-}

module Omnifmt.Directory (
    -- * Changing directories
    withCurrentDirectory,
) where

import Control.Monad.Catch  (MonadMask, bracket)
import Control.Monad.Except

import System.Directory.Extra hiding (withCurrentDirectory)

-- | @withCurrentDirectory dir action@ performs @action@ with the current directory set to @dir@.
--   The current directory is reset back to what it was afterwards.
withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory)
    $ \_ -> liftIO (setCurrentDirectory dir) >> action

