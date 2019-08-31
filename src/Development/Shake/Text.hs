{-| Shake functions that have a "Data.Text"-friendly interface -}
module Development.Shake.Text where

import Prelude hiding (readFile, writeFile)
import Control.Monad.IO.Class
import Data.Text
import Data.Text.IO
import Development.Shake
import qualified System.Exit as Exit

-- | See 'readFile'.
readFileT :: FilePath -> Action Text
readFileT f = need [f] >> liftIO (readFile f)

-- | See 'writeFile'.
writeFileT :: MonadIO m => FilePath -> Text -> m ()
writeFileT name f = liftIO $ writeFile name f

-- | Abort the build with given message and exit code 1.
--
-- See 'Exit.die'.
die :: Text -> Action a
die = liftIO . Exit.die . unpack
