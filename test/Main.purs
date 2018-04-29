module Test.Main where

import Prelude
import Plan.Trans (runPlanT, initRouteRef, respond, paramPattern, reply)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Either (Either (..))
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, message)

main :: Eff (console :: CONSOLE, ref :: REF, exception :: EXCEPTION) Unit
main = do
  routeRef <- initRouteRef
  runPlanT routeRef $ do
     respond (paramPattern "test") $ do
       pure "test Ok"

     ret <- reply unit "test"
     case ret of
       Left e -> liftEff $ log $ message e
       Right r -> liftEff $ log r
