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
import Browser.Events as BE
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
    , detailBreed : Maybe Breed
    , clickedOn : String
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
    Model Loading [] Nothing "loading"


failureModel : Model
failureModel =
    Model Failure
        []
        Nothing
        "failure"


init : () -> ( Model, Cmd Msg )
init _ =
    ( loadingModel, getBreeds )



-- UPDATE


type Msg
    = Reload
    | GotBreeds (Result Http.Error (List Breed))
    | GoDetails String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoDetails str ->
            ( { model | clickedOn = Debug.log "go details" str }, Cmd.none )

        Reload ->
            ( loadingModel, getBreeds )

        GotBreeds result ->
            case result of
                Ok breeds ->
                    ( Model Success breeds Nothing "got breeds", Cmd.none )

                Err _ ->
                    ( failureModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin-left" "20px" ]
        [ h2 [] [ text "Dog Breeds" ]
        , viewBreeds model
        ]


viewBreeds : Model -> Html Msg
viewBreeds model =
    case model.state of
        Failure ->
            div []
                [ text "I could not load the breeds for some reason. "
                , button [ HE.onClick Reload ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ div [ class "breedTableDiv" ]
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
                                    tr []
                                        [ td
                                            [ class "linkText"
                                            , value b.name
                                            , HE.on "click" <| D.map GoDetails HE.targetValue
                                            ]
                                            [ text <| b.name ]
                                        , td [] [ text subBreedsTxt ]
                                        ]
                                )
                                model.allBreeds
                            )
                        ]
                    ]
                ]



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
