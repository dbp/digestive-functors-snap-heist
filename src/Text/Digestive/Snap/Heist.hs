-- | Module providing a snap backend to work with heist for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Text.Digestive.Snap.Heist
    ( HeistView
    , SnapForm
    , input
    , inputRead
    , errors
    , childErrors
    , snapEnvironment
    , eitherSnapForm
    , runFormHeist
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
import Text.Digestive.Transform
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

-- HeistView has mapping from Ids to Names, then from Ids to Values, then Ids to Errors
data HeistView = HeistView (M.Map FormId Text) (M.Map FormId [Text]) (M.Map FormId [Text])

instance Monoid HeistView where
  mempty = HeistView M.empty M.empty M.empty
  (HeistView idNameMap1 nV1 nE1) `mappend` (HeistView idNameMap2 nV2 nE2) = 
    HeistView (idNameMap1 `mappend` idNameMap2) (M.unionWith (++) nV1 nV2) (M.unionWith (++) nE1 nE2)


input :: (Monad m, Functor m)
         => Text -> Formlet m SnapEnv Text HeistView String
input name def = Form $ do
    allinp <- do env <- ask
                 case env of Environment f -> lift $ lift $ f $ zeroId "" -- snap-heist backend gives all regardless of id
                             NoEnvironment -> return Nothing
    let val = fmap (B8.unpack . BS.concat) $  join $ fmap (M.lookup (TE.encodeUtf8 name) . unSnapEnv) allinp
    id' <- getFormId
    let view' =  HeistView (M.insert id' name M.empty) (M.insert id' [(T.pack $ fromMaybe "" (val `mplus` def))] M.empty) M.empty
        result' = Ok $ fromMaybe "" $  val
    return (View (const view'), result')


inputRead :: (Monad m, Functor m, Show a, Read a)
          => Text
          -> Text
          -> Formlet m SnapEnv Text HeistView a
inputRead name error' def = input name (fmap show def) `transform` transformRead error'
  
{-inputCheckBox :: (Monad m, Functor m, FormInput i f)
              => Text
              -> Bool
              -> Form m i e [(Text, Splice m)] Bool
inputCheckBox name inp = flip Forms.inputBool inp $ \id' inp' ->
  [(T.concat [name, "-", "value"], return [X.TextNode "checked"])]
-}


errorList :: FormId -> [Text] -> HeistView
errorList id' errors' = HeistView M.empty M.empty (M.insert id' errors' M.empty)

errors :: Monad m
       => Form m i Text HeistView ()
errors = Form $ do
    range <- getFormRange
    id' <- getFormId
    return (View (errorList id' . retainErrors range), Ok ())

childErrors :: Monad m
            => Form m i Text HeistView ()
childErrors = Form $ do
    range <- getFormRange
    id' <- getFormId
    return (View (errorList id' . retainChildErrors range), Ok ())


runFormHeist :: Monad m
        => Form m i Text HeistView a            -- ^ Form to run
        -> String                    -- ^ Identifier for the form
        -> Environment m i           -- ^ Input environment
        -> m (View Text [(Text,Splice m)], Result Text a)  -- ^ Result
runFormHeist form id' env = do (view', result) <- evalStateT (runReaderT (unForm form) env) $ unitRange $ zeroId id'
                               return (View $ \e -> mapIdsSplices $ unView view' e, result)

mapIdsSplices (HeistView idsToNames idVals idErrs) = catMaybes $ valueSplices idVals ++ errorSplices idErrs
  where valueSplices vs = map valueSplice $ M.assocs vs
        valueSplice (id',texts) = fmap (\n -> (T.concat [n, "-value"], return [X.TextNode  (T.concat texts)])) $ M.lookup id' idsToNames
        errorSplices es = map errorSplice $ M.assocs es
        errorSplice (id',errs) = fmap (\n -> (T.concat [n, "-errors"], mapSplices eS errs)) $ M.lookup id' idsToNames
        eS error = runChildrenWithText [("error", error)]
        

-- | Environment that will send back all the parameters parsed by Snap
-- does not care what it was asked for.
snapEnvironment :: (MonadSnap m) => Environment m SnapEnv
snapEnvironment = Environment $ \_ -> do
    env <- getParams
    return $ Just $ SnapEnv env

-- | Run a snap form
--
-- * When we are responding to a GET request, you will simply receive splices with default values
--
-- * When we are responding to another request method, the form data will be
--   used. When errors occur, you will receive the error and value splices, otherwise,
--   you will get the actual result
--
eitherSnapForm :: (MonadSnap m)
               => SnapForm m Text HeistView a  -- ^ Form
               -> String            -- ^ Form name
               -> m (Either [(Text,Splice m)] a)    -- ^ Result
eitherSnapForm form name = do
    method' <- rqMethod <$> getRequest
    case method' of GET -> liftM Left $ viewFormHeist form name
                    _   -> eitherFormHeist form name snapEnvironment

eitherFormHeist :: Monad m
                => Form m i Text HeistView a   -- ^ Form to run
                -> String           -- ^ Identifier for the form
                -> Environment m i  -- ^ Input environment
                -> m (Either [(Text,Splice m)] a)   -- ^ Result
eitherFormHeist form id' env = do
    (view', result) <- runFormHeist form id' env
    return $ case result of Error e  -> Left $ unView view' e
                            Ok x     -> Right x

viewFormHeist :: Monad m
              => Form m i Text HeistView a  -- ^ Form to run
              -> String          -- ^ Identifier for the form
              -> m [(Text,Splice m)]            -- ^ Result
viewFormHeist form id' = do
    (view', _) <- runFormHeist form id' NoEnvironment
    return $ unView view' []
