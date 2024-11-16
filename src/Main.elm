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
    , update
    , view
    , viewBreeds
    )

import Array as A exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Http
import Json.Decode as D exposing (Decoder)
import List as L exposing (..)
import Maybe.Extra as MX


fetchAllBreedsUrl : String
fetchAllBreedsUrl =
    "https://dog.ceo/api/breeds/list/all"


fetchImageUrls : String -> String
fetchImageUrls breedName =
    "https://dog.ceo/api/breed/" ++ breedName ++ "/images"



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { state : AppState
    , allBreeds : Dict String Breed
    , detailBreed : Maybe Breed
    , pageStartIdx : Int
    }


type AppState
    = Failure String
    | Loading
    | Success


type alias Breed =
    { name : String
    , subBreeds : List String
    , imageUrls : Array String
    }


loadingModel : Model
loadingModel =
    Model Loading Dict.empty Nothing 0


failureModel : String -> Model
failureModel errMsg =
    Model (Failure errMsg) Dict.empty Nothing 0


init : () -> ( Model, Cmd Msg )
init _ =
    ( loadingModel, getBreeds )



-- UPDATE


type Msg
    = Reload
    | GotBreeds (Result Http.Error (Dict String Breed))
    | GoDetails String
    | ReturnToBreedsList
    | GotImageUrls (Result Http.Error (Array String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoDetails str ->
            let
                foundDetailBreedMay =
                    Dict.get str model.allBreeds

                needToFetchImageUrls =
                    Maybe.map (.imageUrls >> A.isEmpty) foundDetailBreedMay == Just True
            in
            ( { model
                -- todo also update breed in allBreeds with imageUrls
                | detailBreed = foundDetailBreedMay
                , state =
                    if needToFetchImageUrls then
                        Loading

                    else
                        model.state
              }
            , if needToFetchImageUrls then
                MX.unwrap Cmd.none (\breed -> getImageUrls breed.name) foundDetailBreedMay

              else
                Cmd.none
            )

        Reload ->
            ( loadingModel, getBreeds )

        GotBreeds result ->
            case result of
                Ok breeds ->
                    ( Model Success (Debug.log "got breeds" breeds) Nothing 0, Cmd.none )

                Err _ ->
                    ( failureModel "Unable to load the breeds.", Cmd.none )

        ReturnToBreedsList ->
            ( { model | detailBreed = Nothing }, Cmd.none )

        GotImageUrls result ->
            case result of
                Ok urls ->
                    let
                        newBreedMay =
                            Maybe.map (\b -> { b | imageUrls = Debug.log "got image urls" urls }) model.detailBreed
                    in
                    -- todo need to update imageUrls in allBreeds as well
                    ( { model | state = Success, detailBreed = newBreedMay, allBreeds = updateAllBreeds newBreedMay model.allBreeds }, Cmd.none )

                Err _ ->
                    ( failureModel <|
                        "Unable to load the images for "
                            ++ MX.unwrap "(unknown)" (\brd -> brd.name) model.detailBreed
                    , Cmd.none
                    )


updateAllBreeds : Maybe Breed -> Dict String Breed -> Dict String Breed
updateAllBreeds newBreedMay allBreeds =
    case newBreedMay of
        Nothing ->
            allBreeds

        Just newBreed ->
            Dict.insert newBreed.name newBreed allBreeds



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Failure errMsg ->
            div []
                [ text errMsg
                , button [ HE.onClick Reload ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success ->
            let
                header =
                    MX.unwrap "Dog Breeds" (\s -> "Details for " ++ s.name) model.detailBreed
            in
            div [ style "margin-left" "20px" ]
                [ h2 [] [ text header ]
                , if MX.isJust model.detailBreed then
                    viewDetails model

                  else
                    viewBreeds model
                ]


viewBreeds : Model -> Html Msg
viewBreeds model =
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
                     <|
                        L.sortBy .name <|
                            Dict.values model.allBreeds
                    )
                ]
            ]
        ]


viewDetails : Model -> Html Msg
viewDetails model =
    let
        numImages =
            A.length (Maybe.withDefault A.empty <| Maybe.map .imageUrls model.detailBreed)

        firstIdxStr =
            String.fromInt (model.pageStartIdx + 1)

        lastIdxStr =
            String.fromInt <| Basics.min numImages <| model.pageStartIdx + 20
    in
    div []
        [ button [ HE.onClick ReturnToBreedsList ] [ text "⬅︎ Return to Dog Breeds List" ]
        , h3 []
            ([ text <|
                "Displaying "
                    ++ firstIdxStr
                    ++ "-"
                    ++ lastIdxStr
                    ++ " of "
                    ++ String.fromInt numImages
                    ++ " images"
             , br [] []
             , br [] []
             ]
                ++ getImagePage (Maybe.withDefault A.empty <| Maybe.map .imageUrls model.detailBreed)
            )
        ]


getImagePage : Array String -> List (Html msg)
getImagePage urls =
    L.map
        (\url ->
            img
                [ src url
                , alt <| "image not loaded: " ++ url
                ]
                []
        )
    <|
        take 20 (A.toList urls)



-- HTTP


getBreeds : Cmd Msg
getBreeds =
    Http.get
        { url = fetchAllBreedsUrl
        , expect = Http.expectJson GotBreeds breedsDecoder
        }


getImageUrls : String -> Cmd Msg
getImageUrls breedName =
    Http.get
        { url = fetchImageUrls breedName
        , expect = Http.expectJson GotImageUrls imageUrlsDecoder
        }


breedsDecoder : Decoder (Dict String Breed)
breedsDecoder =
    let
        breedsDictDecoder =
            D.map (Dict.map mkBreed) (D.dict (D.list D.string))
    in
    D.field "message" breedsDictDecoder


mkBreed : String -> List String -> Breed
mkBreed name subBreeds =
    Breed name subBreeds A.empty


imageUrlsDecoder : Decoder (Array String)
imageUrlsDecoder =
    D.field "message" <| D.map A.fromList <| D.list D.string
