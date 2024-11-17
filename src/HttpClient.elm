module HttpClient exposing
    ( getBreeds
    , getImageUrls
    )

import Array as A exposing (Array)
import Common
import Dict exposing (Dict)
import Http
import Json.Decode as D exposing (Decoder)


fetchAllBreedsUrl : String
fetchAllBreedsUrl =
    "https://dog.ceo/api/breeds/list/all"


fetchImageUrls : String -> String
fetchImageUrls breedName =
    "https://dog.ceo/api/breed/" ++ breedName ++ "/images"


getBreeds : Cmd Common.Msg
getBreeds =
    Http.get
        { url = fetchAllBreedsUrl
        , expect = Http.expectJson Common.GotBreeds breedsDecoder
        }


getImageUrls : String -> Cmd Common.Msg
getImageUrls breedName =
    Http.get
        { url = fetchImageUrls breedName
        , expect = Http.expectJson Common.GotImageUrls imageUrlsDecoder
        }


breedsDecoder : Decoder (Dict String Common.Breed)
breedsDecoder =
    let
        breedsDictDecoder =
            D.map (Dict.map mkBreed) (D.dict (D.list D.string))
    in
    D.field "message" breedsDictDecoder


mkBreed : String -> List String -> Common.Breed
mkBreed name subBreeds =
    Common.Breed name subBreeds A.empty


imageUrlsDecoder : Decoder (Array String)
imageUrlsDecoder =
    D.field "message" <| D.map A.fromList <| D.list D.string
