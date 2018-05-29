module Plan.Trans
  ( Param (..)
  , ActionT
  , options
  , params
  , param

  , Pattern (..)
  , regexPattern
  , regexPattern'
  , paramPattern
  , mkParamPattern_
  , mkParamPattern
  , paramPattern_
  , unsafeRegex

  , RouteRef
  , initRouteRef
  , PlanT
  , runPlanT
  , respond
  , reply
  ) where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Effect.Ref (Ref, new, read, modify)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..), fromRight, note)
import Data.Newtype (class Newtype)
import Data.Array (tail, mapWithIndex, zipWith, concat, findMap)
import Data.Array.NonEmpty (catMaybes)
import Effect.Exception (Error, error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Data.String.Regex (Regex, match, regex, replace)
import Data.String.Regex.Flags (noFlags, global, RegexFlags)
import Data.String (takeWhile, drop, codePointFromChar)
import Data.Tuple (Tuple (..), snd, fst)

data Param = Param String String

derive instance eqParam :: Eq Param
derive instance ordParam :: Ord Param

newtype ActionT opts m a = ActionT (ExceptT Error (ReaderT (Tuple opts (Array Param)) m) a)

runActionT :: forall opts m a. opts -> Array Param -> ActionT opts m a -> m (Either Error a)
runActionT opts p (ActionT m)= flip runReaderT (Tuple opts p) $ runExceptT m

derive instance newtypeActionT :: Newtype (ActionT opts m a) _

instance functorActionT :: Functor m => Functor (ActionT opts m) where
  map f (ActionT m) = ActionT $ map f m

instance applyActionT :: Monad m => Apply (ActionT opts m) where
  apply = ap

instance applicativeActionT :: Monad m => Applicative (ActionT opts m) where
  pure = ActionT <<< pure

instance bindActionT :: Monad m => Bind (ActionT opts m) where
  bind (ActionT m) k = ActionT $ do
    a <- m
    case k a of
      ActionT b -> b

instance monadActionT :: Monad m => Monad (ActionT opts m)

instance monadTransActionT :: MonadTrans (ActionT opts) where
  lift = ActionT <<< lift <<< lift

instance monadEffActionT :: MonadEffect m => MonadEffect (ActionT opts m) where
  liftEffect = lift <<< liftEffect

instance monadAffActionT :: MonadAff m => MonadAff (ActionT opts m) where
  liftAff = lift <<< liftAff

options :: forall opts m. Monad m => ActionT opts m opts
options = ActionT $ fst <$> ask

params :: forall opts m. Monad m => ActionT opts m (Array Param)
params = ActionT $ snd <$> ask

param :: forall opts m. Monad m => String -> ActionT opts m String
param k = ActionT $ except <<< note err <<< findMap doMap =<< snd <$> ask
  where doMap :: Param -> Maybe String
        doMap (Param k0 v) | k0 == k = Just v
                           | otherwise = Nothing

        err = error $ "param: " <> k <> " is required"

newtype Pattern = Pattern (String -> Maybe (Array Param))

derive instance newtypePattern :: Newtype Pattern _

regexPattern :: Regex -> Pattern
regexPattern reg = Pattern go
  where go :: String -> Maybe (Array Param)
        go xs = do
          m <- match reg xs
          pure $ mapWithIndex toParam $ catMaybes m
          where toParam :: Int -> String -> Param
                toParam idx v = Param (show idx) v

unsafeRegex :: String -> RegexFlags -> Regex
unsafeRegex re = unsafePartial $ fromRight <<< regex re

regexPattern' :: String -> Pattern
regexPattern' = regexPattern <<< flip unsafeRegex noFlags

paramPattern :: String -> Pattern
paramPattern = mkParamPattern go ":[^:]+:" "(.+)"
  where go :: String -> String
        go = takeWhile (_ /= codePointFromChar ':') <<< drop 1

mkParamPattern :: (String -> String) -> String -> String -> String -> Pattern
mkParamPattern preprocess spec target xs = mkParamPattern_ keys reg
  where specReg = unsafeRegex spec global
        reg = unsafeRegex ("^" <> replace specReg target xs <> "$") noFlags
        keys = case (catMaybes <$> match specReg xs) of
                 Nothing -> []
                 Just v -> map preprocess v

mkParamPattern_ :: Array String -> Regex -> Pattern
mkParamPattern_ keys reg = paramPattern_ keys go
  where go :: String -> Maybe (Array String)
        go ys = tail =<< catMaybes <$> match reg ys

paramPattern_ :: Array String -> (String -> Maybe (Array String)) -> Pattern
paramPattern_ keys values = Pattern $ \xs -> zipWith Param keys <$> values xs

data Route opts m a = Route Pattern (ActionT opts m a)

newtype RouteRef opts m a = RouteRef (Ref (Array (Route opts m a)))
derive instance newtypeRouteRef :: Newtype (RouteRef opts m a) _

initRouteRef :: forall opts m a. Effect (RouteRef opts m a)
initRouteRef = map RouteRef $ new []

addRoute :: forall opts m a. RouteRef opts m a -> Route opts m a -> Effect Unit
addRoute (RouteRef ref) x = void $ modify (\xs -> concat [xs, [x]]) ref

routes :: forall opts m a. RouteRef opts m a -> Effect (Array (Route opts m a))
routes (RouteRef ref) = read ref

newtype PlanT opts a m b = PlanT (ReaderT (RouteRef opts m a) m b)

runPlanT :: forall opts a b m. Monad m => RouteRef opts m a -> PlanT opts a m b -> m b
runPlanT ref (PlanT m) = runReaderT m ref

derive instance newtypePlanT :: Newtype (PlanT opts a m b) _

instance functorPlanT :: Functor m => Functor (PlanT opts a m) where
  map f (PlanT m) = PlanT $ map f m

instance applyPlanT :: Monad m => Apply (PlanT opts a m) where
  apply = ap

instance applicativePlanT :: Monad m => Applicative (PlanT opts a m) where
  pure = PlanT <<< pure

instance bindPlanT :: Monad m => Bind (PlanT opts a m) where
  bind (PlanT m) k = PlanT $ do
    a <- m
    case k a of
      PlanT b -> b

instance monadPlanT :: Monad m => Monad (PlanT opts a m)

instance monadTransPlanT :: MonadTrans (PlanT opts a) where
  lift = PlanT <<< lift

instance monadEffPlanT :: MonadEffect m => MonadEffect (PlanT opts a m) where
  liftEffect = lift <<< liftEffect

instance monadAffPlanT :: MonadAff m => MonadAff (PlanT opts a m) where
  liftAff = lift <<< liftAff

instance monadAskPlanT :: Monad m => MonadAsk (RouteRef opts m a) (PlanT opts a m) where
  ask = PlanT ask

respond :: forall opts a m. MonadEffect m => Pattern -> ActionT opts m a -> PlanT opts a m Unit
respond pat action = liftEffect <<< flip addRoute (Route pat action) =<< ask

data MatchRoute opts m a = MatchRoute (Array Param) (ActionT opts m a)

matchRoute :: forall opts m a. String -> Array (Route opts m a) -> Maybe (MatchRoute opts m a)
matchRoute xs = findMap doMap
  where doMap :: forall opts0 m0 a0. Route opts0 m0 a0 -> Maybe (MatchRoute opts0 m0 a0)
        doMap (Route (Pattern f) m) = do
           p <- f xs
           pure $ MatchRoute p m

reply :: forall opts a m. MonadEffect m => opts -> String -> PlanT opts a m (Either Error a)
reply opts xs = do
  ref <- ask
  rs <- liftEffect $ routes ref
  case matchRoute xs rs of
    Nothing -> pure $ Left $ error "route not found."
    Just (MatchRoute ps m) -> lift $ runActionT opts ps m
