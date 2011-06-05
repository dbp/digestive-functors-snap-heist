-- | Module providing a snap backend to work with heist for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Text.Digestive.Backend.Snap.Heist
    ( SnapEnv
    , unSnapEnv
    , SnapForm
    , snapEnvironment
    , eitherSnapForm
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Data.ByteString as SB
import Data.ByteString.UTF8 as SB (toString, fromString)
import Snap.Types

import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)

newtype SnapEnv = SnapEnv {unSnapEnv :: Params}

instance FormInput SnapEnv () where
    getInputStrings = const [] -- we do not support this, as our "input" is the whole environment
    getInputFile = const Nothing

-- | Simplification of the `Form` type, instantiated to Snap
--
type SnapForm m = Form m SnapEnv

-- | Environment that will send back all the parameters parsed by Snap
-- does not care what it was asked for.
snapEnvironment :: (MonadSnap m) => Environment m SnapEnv
snapEnvironment = Environment $ \_ -> do
    env <- getParams
    return $ Just $ SnapEnv env

-- | Run a snap form
--
-- * When we are responding to a GET request, you will simply receive the form
--   as a view
--
-- * When we are responding to another request method, the form data will be
--   used. When errors occur, you will receive the form as a view, otherwise,
--   you will get the actual result
--
eitherSnapForm :: (MonadSnap m)
               => SnapForm m e v a  -- ^ Form
               -> String            -- ^ Form name
               -> m (Either v a)    -- ^ Result
eitherSnapForm form name = do
    method' <- rqMethod <$> getRequest
    case method' of GET -> liftM Left $ viewForm form name
                    _   -> eitherForm form name snapEnvironment
