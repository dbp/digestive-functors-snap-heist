-- | Module providing a snap backend to work with heist for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Text.Digestive.Snap.Heist
    ( HeistView
    , SnapForm
    , input
    , errors
    , childErrors
    , snapEnvironment
    , eitherSnapForm
    , runForm
    ) where


import Control.Monad (forM_, unless, when, join, mplus, liftM)
import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (mempty)
import qualified Data.Map as M

import qualified Text.XmlHtml as X
import Text.Templating.Heist
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Text.Digestive.Types hiding (viewForm, eitherForm)
import Text.Digestive.Result
import Text.Digestive.Forms (FormInput (..))
import qualified Text.Digestive.Forms as Forms
import qualified Text.Digestive.Common as Common
import Text.Digestive.Forms.Html


import Control.Applicative ((<$>))

import Snap.Types


newtype SnapEnv = SnapEnv {unSnapEnv :: Params}

instance FormInput SnapEnv () where
    getInputStrings = const [] -- we do not support this, as our "input" is the whole environment
    getInputFile = const Nothing

-- | Simplification of the `Form` type, instantiated to Snap
--
type SnapForm m = Form m SnapEnv


data HeistView m = HeistView (M.Map FormId Text) [(FormId, (Text -> (Text, Splice m)))]

instance (Monad m) => Monoid (HeistView m) where
  mempty = HeistView M.empty []
  (HeistView idNameMap1 idSplice1) `mappend` (HeistView idNameMap2 idSplice2) = 
    HeistView (idNameMap1 `mappend` idNameMap2) (idSplice1 `mappend` idSplice2)


input :: (Monad m, Functor m)
         => Text -> Formlet m SnapEnv Text (HeistView m) String
input name def = Form $ do
    allinp <- do env <- ask
                 case env of Environment f -> lift $ lift $ f $ zeroId "" -- since the snap-heist backend gives all regardless, any id
                             NoEnvironment -> return Nothing
    let val = fmap (B8.unpack . BS.concat) $  join $ fmap (M.lookup (TE.encodeUtf8 name) . unSnapEnv) allinp
    id' <- getFormId
    let view' = HeistView (M.insert id' name M.empty) [(id', \n -> (T.concat [n, "-", "value"], return [X.TextNode (T.pack $ fromMaybe "" (val `mplus` def))]))]
        result' = Ok $ fromMaybe "" $  val
    return (View (const $ view'), result')


{-inputRead :: (Monad m, Functor m, FormInput i f, Show a, Read a)
          => Text
          -> e
          -> Maybe a
          -> Form m i e [(Text, Splice m)] a
inputRead name error' = flip Forms.inputRead error' $ \id' inp ->
  [(T.concat [name, "-", "value"], return [X.TextNode (T.pack $ fromMaybe "" inp)])] 
  
inputCheckBox :: (Monad m, Functor m, FormInput i f)
              => Text
              -> Bool
              -> Form m i e [(Text, Splice m)] Bool
inputCheckBox name inp = flip Forms.inputBool inp $ \id' inp' ->
  [(T.concat [name, "-", "value"], return [X.TextNode "checked"])]
-}

errorSplice :: Monad m => Text -> Splice m
errorSplice error = runChildrenWithText [("error", error)]

errorList :: Monad m => FormId -> [Text] -> (HeistView m)
errorList id' errors' = HeistView M.empty [(id', \n -> (T.concat [n, "-", "error"], mapSplices errorSplice errors'))]


errors :: Monad m
       => Form m i Text (HeistView m) ()
errors = Form $ do
    range <- getFormRange
    id' <- getFormId
    return (View (errorList id' . retainErrors range), Ok ())

childErrors :: Monad m
            => Form m i Text (HeistView m) ()
childErrors = Form $ do
    range <- getFormRange
    id' <- getFormId
    return (View (errorList id' . retainChildErrors range), Ok ())


runFormHeist :: Monad m
        => Form m i Text (HeistView m) a            -- ^ Form to run
        -> String                    -- ^ Identifier for the form
        -> Environment m i           -- ^ Input environment
        -> m (View Text [(Text,Splice m)], Result Text a)  -- ^ Result
runFormHeist form id' env = do (view', result) <- evalStateT (runReaderT (unForm form) env) $ unitRange $ zeroId id'
                               return (View $ \e -> mapIds $ unView view' e, result)

mapIds (HeistView idsToNames idsAndSplices) = catMaybes $ map (\(i,mkS) -> liftM mkS $ M.lookup i idsToNames) idsAndSplices


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
               => SnapForm m Text (HeistView m) a  -- ^ Form
               -> String            -- ^ Form name
               -> m (Either [(Text,Splice m)] a)    -- ^ Result
eitherSnapForm form name = do
    method' <- rqMethod <$> getRequest
    case method' of GET -> liftM Left $ viewForm form name
                    _   -> eitherForm form name snapEnvironment

eitherForm :: Monad m
           => Form m i Text (HeistView m) a   -- ^ Form to run
           -> String           -- ^ Identifier for the form
           -> Environment m i  -- ^ Input environment
           -> m (Either [(Text,Splice m)] a)   -- ^ Result
eitherForm form id' env = do
    (view', result) <- runFormHeist form id' env
    return $ case result of Error e  -> Left $ unView view' e
                            Ok x     -> Right x

viewForm :: Monad m
         => Form m i Text (HeistView m) a  -- ^ Form to run
         -> String          -- ^ Identifier for the form
         -> m [(Text,Splice m)]            -- ^ Result
viewForm form id' = do
    (view', _) <- runFormHeist form id' NoEnvironment
    return $ unView view' []
