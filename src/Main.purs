module Main where

import Prelude
import Effect (Effect)
import Pha.App (sandbox)
import Lightsout.Model (init, update)
import Lightsout.View (view)

main :: Effect Unit
main = sandbox {init, view, update, selector: "#root"}