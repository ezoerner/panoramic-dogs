-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--


module Main exposing
    ( AppState(..)
    , Breed
    , Model
    , Msg(..)
    , breedsDecoder
    , failureModel
    , getBreeds
    , init
    , loadingModel
    , main
    , subBreedsToBreed
    , subscriptions
    , update
    , view
    , viewBreeds
    )

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Http
import Json.Decode as D exposing (Decoder)
import List as L exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { state : AppState
    , allBreeds : List Breed
    , currBreed : Maybe Breed
    }


type AppState
    = Failure
    | Loading
    | Success


type alias Breed =
    { name : String
    , subBreeds : List String
    }


loadingModel : Model
loadingModel =
    { state = Loading, allBreeds = [], currBreed = Nothing }


failureModel : Model
failureModel =
    { state = Failure, allBreeds = [], currBreed = Nothing }


init : () -> ( Model, Cmd Msg )
init _ =
    ( loadingModel, getBreeds )



-- UPDATE


type Msg
    = MorePlease
    | GotBreeds (Result Http.Error (List Breed))
    | SelectBreed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( loadingModel, getBreeds )

        GotBreeds result ->
            case result of
                Ok breeds ->
                    case head breeds of
                        Just breed ->
                            ( Model Success breeds (Just breed), Cmd.none )

                        Nothing ->
                            ( failureModel, Cmd.none )

                Err _ ->
                    ( failureModel, Cmd.none )

        SelectBreed breedName ->
            case head <| L.filter (\b -> String.startsWith b.name breedName) model.allBreeds of
                Nothing ->
                    ( failureModel, Cmd.none )

                Just breed ->
                    ( Model Success model.allBreeds (Just breed), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin-left" "20px" ]
        [ h2 [] [ text "Dog Breed List" ]
        , viewBreeds model
        ]


viewBreeds : Model -> Html Msg
viewBreeds model =
    case model.state of
        Failure ->
            div []
                [ text "I could not load the breeds for some reason. "
                , button [ HE.onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ button [ HE.onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , div [ class "breedTableDiv" ]
                    [ table []
                        [ thead [] [ tr [] [ th [ class "breedCol" ] [ text "Breed" ], th [] [ text "Sub-Breeds" ] ] ]
                        , tbody [ class "breedTBody" ]
                            (L.map
                                (\b ->
                                    let
                                        subBreedsTxt =
                                            if isEmpty b.subBreeds then
                                                ""

                                            else
                                                String.concat <| intersperse ", " b.subBreeds
                                    in
                                    tr [] [ td [ class "breedText" ] [ text <| b.name ], td [] [ text subBreedsTxt ] ]
                                )
                                model.allBreeds
                            )
                        ]
                    ]
                ]



{- h3 [] [ text "Breeds (sub-breeds):" ]
   , select [ HE.onInput SelectBreed ]
       (L.map
           (\b ->
               let
                   subBreedsTxt =
                       if isEmpty b.subBreeds then
                           ""

                       else
                           " (" ++ String.fromInt (length b.subBreeds) ++ ")"
               in
               option
                   [ value b.name
                   , selected <| model.currBreed == Just b
                   ]
                   [ text <| b.name ++ subBreedsTxt ]
           )
           model.allBreeds
       )
   , h3 [] [ text "Sub-Breeds:" ]
   , ul []
       (case model.currBreed of
           Nothing ->
               []

           Just brd ->
               L.map (\sb -> li [] [ text sb ]) brd.subBreeds
       )
-}
-- HTTP


getBreeds : Cmd Msg
getBreeds =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = Http.expectJson GotBreeds breedsDecoder
        }


breedsDecoder : Decoder (List Breed)
breedsDecoder =
    let
        breedsDictDecoder =
            D.map (Dict.map subBreedsToBreed) (D.dict (D.list D.string))
    in
    D.field "message" <| D.map Dict.values breedsDictDecoder


subBreedsToBreed : String -> List String -> Breed
subBreedsToBreed name subBreeds =
    Breed name subBreeds
