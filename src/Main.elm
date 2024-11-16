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
import String as S exposing (String)


fetchAllBreedsUrl : String
fetchAllBreedsUrl =
    "https://dog.ceo/api/breeds/list/all"


fetchImageUrls : String -> String
fetchImageUrls breedName =
    "https://dog.ceo/api/breed/" ++ breedName ++ "/images"


numImagesPerPage : Int
numImagesPerPage =
    20



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
    | NextPage
    | PrevPage


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
            ( { model | detailBreed = Nothing, pageStartIdx = 0 }, Cmd.none )

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

        NextPage ->
            ( { model | pageStartIdx = model.pageStartIdx + numImagesPerPage }, Cmd.none )

        PrevPage ->
            ( { model | pageStartIdx = model.pageStartIdx - numImagesPerPage }, Cmd.none )


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
    let
        headerHtml =
            h2 [] [ text <| MX.unwrap "Dog Breeds" .name model.detailBreed ]
    in
    div [ style "margin-left" "20px" ]
        [ headerHtml
        , case model.state of
            Failure errMsg ->
                div []
                    [ text errMsg
                    , br [] []
                    , br [] []
                    , button [ HE.onClick Reload ] [ b [] [ text "Try Again" ] ]
                    ]

            Loading ->
                text "Loading..."

            Success ->
                if MX.isJust model.detailBreed then
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
                                        S.concat <| intersperse ", " b.subBreeds
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
            A.length
                (Maybe.withDefault A.empty <|
                    Maybe.map .imageUrls model.detailBreed
                )

        firstIdx =
            model.pageStartIdx + 1

        lastIdx =
            Basics.min numImages <| model.pageStartIdx + numImagesPerPage
    in
    div []
        [ button [ HE.onClick ReturnToBreedsList ] [ text "<<< Return to Dog Breeds List" ]
        , hr [] []
        , h3 []
            [ text <|
                "Displaying "
                    ++ S.fromInt firstIdx
                    ++ "-"
                    ++ S.fromInt lastIdx
                    ++ " of "
                    ++ S.fromInt numImages
                    ++ " images"
            ]
        , br [] []
        , br [] []
        , pagingButtons firstIdx lastIdx numImages
        , imagePage
            firstIdx
            lastIdx
            (Maybe.withDefault A.empty <| Maybe.map .imageUrls model.detailBreed)
        ]


pagingButtons : Int -> Int -> Int -> Html Msg
pagingButtons firstIdx lastIdx numImages =
    let
        shouldDisplayPaging =
            numImages > numImagesPerPage

        prevDisabled =
            firstIdx == 1

        nextDisabled =
            lastIdx == numImages
    in
    div [] <|
        if shouldDisplayPaging then
            [ button [ class "previous", disabled prevDisabled, HE.onClick PrevPage ] [ text "« Previous" ]
            , button [ class "next", disabled nextDisabled, HE.onClick NextPage ] [ text "Next »" ]
            , br [] []
            , br [] []
            ]

        else
            []


imagePage : Int -> Int -> Array String -> Html Msg
imagePage firstIdx lastIdx urls =
    div [] <|
        L.map
            (\url ->
                img
                    [ src url
                    , alt <| "image not loaded: " ++ url
                    ]
                    []
            )
        <|
            A.toList <|
                A.slice (firstIdx - 1) lastIdx urls



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
