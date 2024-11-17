module Main exposing (main)

import Browser
import Common
import Model exposing (Model)
import Update
import View


main : Program () Model Common.Msg
main =
    Browser.element
        { init = Model.init
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        , view = View.view
        }
