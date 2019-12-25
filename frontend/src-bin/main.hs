module Main where

import Frontend (frontend)
import Common.Route (fullRouteEncoder)
import Obelisk.Frontend (runFrontend)
import Obelisk.Route (checkEncoder)
import Reflex.Dom (run)

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
