module Common exposing (Breed, Msg(..), numImagesPerPage)

import Array exposing (Array)
import Dict exposing (Dict)
import Http


numImagesPerPage : Int
numImagesPerPage =
    20


type Msg
    = Reload
    | GotBreeds (Result Http.Error (Dict String Breed))
    | GoDetails String
    | ReturnToBreedsList
    | GotImageUrls (Result Http.Error (Array String))
    | NextPage
    | PrevPage


type alias Breed =
    { name : String
    , subBreeds : List String
    , imageUrls : Array String
    }
