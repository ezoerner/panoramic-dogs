module Model exposing (AppState(..), Model, failureModel, init, loadingModel)

import Common
import Dict exposing (Dict)
import HttpClient as HC


type alias Model =
    { state : AppState
    , allBreeds : Dict String Common.Breed
    , detailBreed : Maybe Common.Breed
    , pageStartIdx : Int
    }


type AppState
    = Failure String
    | Loading
    | Success


loadingModel : Model
loadingModel =
    Model Loading Dict.empty Nothing 0


failureModel : String -> Model
failureModel errMsg =
    Model (Failure errMsg) Dict.empty Nothing 0


init : () -> ( Model, Cmd Common.Msg )
init _ =
    ( loadingModel, HC.getBreeds )
