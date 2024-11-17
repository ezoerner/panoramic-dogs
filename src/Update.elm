module Update exposing (update)

import Array as A
import Common
import Dict exposing (Dict)
import HttpClient as HC
import Maybe.Extra as MX
import Model exposing (Model)


update : Common.Msg -> Model -> ( Model, Cmd Common.Msg )
update msg model =
    case msg of
        Common.GoDetails str ->
            let
                foundDetailBreedMay =
                    Dict.get str model.allBreeds

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

        Common.Reload ->
            ( Model.loadingModel, HC.getBreeds )

        Common.GotBreeds result ->
            case result of
                Ok breeds ->
                    ( Model Model.Success (Debug.log "got breeds" breeds) Nothing 0, Cmd.none )

                Err _ ->
                    ( Model.failureModel "Unable to load the breeds.", Cmd.none )

        Common.ReturnToBreedsList ->
            ( { model | detailBreed = Nothing, pageStartIdx = 0 }, Cmd.none )

        Common.GotImageUrls result ->
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

        Common.NextPage ->
            ( { model | pageStartIdx = model.pageStartIdx + Common.numImagesPerPage }, Cmd.none )

        Common.PrevPage ->
            ( { model | pageStartIdx = model.pageStartIdx - Common.numImagesPerPage }, Cmd.none )


updateAllBreeds : Maybe Common.Breed -> Dict String Common.Breed -> Dict String Common.Breed
updateAllBreeds newBreedMay allBreeds =
    case newBreedMay of
        Nothing ->
            allBreeds

        Just newBreed ->
            Dict.insert newBreed.name newBreed allBreeds
