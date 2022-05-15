module Lightsout.View where

import Prelude

import Data.Array ((!!), mapWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Lightsout.Model (Model, Msg(..))
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)

square ∷ ∀a. Boolean → Boolean → Array (H.Prop a) → Html a
square light cross props = 
  H.div ([H.class_ "lightsout-square"] <> props)
  [ H.div [H.class_ "lightsout-square-inner", H.class' "white" light]
    [ H.div [H.class_ "lightsout-square-white"] $ 
        if cross then [
          H.text "1"
        ] else []
    ,   H.div [H.class_ "lightsout-square-black"] $
        if cross then [
          H.text "1"
        ] else []
    ]
  ]

view ∷ Model → Html Msg
view {nrows, ncols, grid, solution} = 
  H.div []
  [ H.button [E.onClick \_ → Solve] [H.text "Solve"]
  , gridView
  ]
  where
  gridView =
    H.div [H.class_ "ui-board"] $ -- <> gridStyle rows columns 4) $
      grid # mapWithIndex \index light →
        let row = index / ncols
            col = index `mod` ncols
            cross = (solution >>= (_ !! index)) == Just true
        in
        square light cross 
        [ H.style "height" $ pc (0.86 / Int.toNumber nrows)
        , H.style "width" $ pc (0.86 / Int.toNumber ncols)
        , H.style "left" $ pc ((Int.toNumber col + 0.07) / Int.toNumber ncols)
        , H.style "top" $ pc ((Int.toNumber row + 0.07) / Int.toNumber nrows)
        , E.onClick \_ → Flip index
        ]