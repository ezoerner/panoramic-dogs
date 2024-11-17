module View exposing (..)

import Array as A exposing (Array)
import Common
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Json.Decode as D
import List as L
import Maybe.Extra as MX
import Model exposing (Model)
import String as S


view : Model -> Html Common.Msg
view model =
    let
        headerHtml =
            h2 [] [ text <| MX.unwrap "Dog Breeds" .name model.detailBreed ]
    in
    div [ style "margin-left" "20px" ]
        [ headerHtml
        , case model.state of
            Model.Failure errMsg ->
                div []
                    [ text errMsg
                    , br [] []
                    , br [] []
                    , button [ HE.onClick Common.Reload ] [ b [] [ text "Try Again" ] ]
                    ]

            Model.Loading ->
                text "Loading..."

            Model.Success ->
                if MX.isJust model.detailBreed then
                    viewDetails model

                else
                    viewBreeds model
        ]


viewBreeds : Model -> Html Common.Msg
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
                                    if L.isEmpty b.subBreeds then
                                        ""

                                    else
                                        S.concat <| L.intersperse ", " b.subBreeds
                            in
                            tr []
                                [ td
                                    [ class "linkText"
                                    , value b.name
                                    , HE.on "click" <| D.map Common.GoDetails HE.targetValue
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


viewDetails : Model -> Html Common.Msg
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
            Basics.min numImages <| model.pageStartIdx + Common.numImagesPerPage
    in
    div []
        [ button [ HE.onClick Common.ReturnToBreedsList ] [ text "<<< Return to Dog Breeds List" ]
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


pagingButtons : Int -> Int -> Int -> Html Common.Msg
pagingButtons firstIdx lastIdx numImages =
    let
        shouldDisplayPaging =
            numImages > Common.numImagesPerPage

        prevDisabled =
            firstIdx == 1

        nextDisabled =
            lastIdx == numImages
    in
    div [] <|
        if shouldDisplayPaging then
            [ button [ class "previous", disabled prevDisabled, HE.onClick Common.PrevPage ] [ text "« Previous" ]
            , button [ class "next", disabled nextDisabled, HE.onClick Common.NextPage ] [ text "Next »" ]
            , br [] []
            , br [] []
            ]

        else
            []


imagePage : Int -> Int -> Array String -> Html Common.Msg
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
