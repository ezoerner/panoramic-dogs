module Update exposing (update)

import Array as A
import Common exposing (..)
import Dict exposing (Dict)
import HttpClient as HC
import Maybe.Extra as MX
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoDetails str ->
            let
                foundDetailBreedMay =
                    Dict.get str model.allBreeds

                -- fetch image urls if detailBreed is present and has empty urls
                needToFetchImageUrls =
                    Maybe.map (.imageUrls >> A.isEmpty) foundDetailBreedMay == Just True
            in
            ( { model
                | detailBreed = foundDetailBreedMay
                , state =
                    if needToFetchImageUrls then
                        Model.Loading

                    else
                        model.state
              }
            , if needToFetchImageUrls then
                MX.unwrap Cmd.none (\breed -> HC.getImageUrls breed.name) foundDetailBreedMay

              else
                Cmd.none
            )

        Reload ->
            ( Model.loadingModel, HC.getBreeds )

        GotBreeds result ->
            case result of
                Ok breeds ->
                    ( Model Model.Success breeds Nothing 0, Cmd.none )

                Err _ ->
                    ( Model.failureModel "Unable to load the breeds.", Cmd.none )

        ReturnToBreedsList ->
            ( { model | detailBreed = Nothing, pageStartIdx = 0 }, Cmd.none )

        GotImageUrls result ->
            case result of
                Ok urls ->
                    let
                        newBreedMay =
                            Maybe.map (\b -> { b | imageUrls = Debug.log "got image urls" urls }) model.detailBreed
                    in
                    ( { model | state = Model.Success, detailBreed = newBreedMay, allBreeds = updateAllBreeds newBreedMay model.allBreeds }, Cmd.none )

                Err _ ->
                    ( Model.failureModel <|
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
