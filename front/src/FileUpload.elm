module FileUpload exposing (image2Url, main)

import Browser
import Env exposing (apiEndpoint)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, map2, string)
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Msg
    = CsvRequested
    | CsvSelected File
    | Uploaded (Result Http.Error Image)
    | Grayscale
    | GrayscaleResponse (Result Http.Error Image)


type alias Model =
    { image : Maybe Image
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { image = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model, Select.file [] CsvSelected )

        CsvSelected file ->
            ( model
            , Http.post
                { url = apiEndpoint ++ "/upload_image"
                , body = Http.multipartBody [ Http.filePart "uploadFile" file ]
                , expect = Http.expectJson Uploaded uploadImageResponseDecorder
                }
            )

        Uploaded result ->
            case result of
                Ok img ->
                    ( { model | image = Just img }, Cmd.none )

                Err err ->
                    ( { model | image = Nothing }, Cmd.none )

        Grayscale ->
            case model.image of
                Just img ->
                    ( model
                    , Http.post
                        { url = apiEndpoint ++ "/grayscale"
                        , body =
                            Http.jsonBody (imageEncoder img)
                        , expect =
                            Http.expectJson GrayscaleResponse
                                grayscaleResponseDecoder
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        GrayscaleResponse result ->
            case result of
                Ok img ->
                    ( { model | image = Just img }, Cmd.none )

                Err err ->
                    ( { model | image = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Image =
    { taskId : String
    , id : String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CsvRequested ] [ text "Upload img" ]
        , button [ onClick Grayscale ] [ text "Grayscale" ]
        , img [ src (image2Url model.image) ] []
        ]


uploadImageResponseDecorder : Decoder Image
uploadImageResponseDecorder =
    field "result" (field "image" imageDecoder)


imageDecoder : Decoder Image
imageDecoder =
    map2 Image
        (field "task_id" string)
        (field "id" string)


grayscaleResponseDecoder : Decoder Image
grayscaleResponseDecoder =
    uploadImageResponseDecorder


image2Url : Maybe Image -> String
image2Url image =
    case image of
        Just img ->
            apiEndpoint ++ "/static/task/" ++ img.taskId ++ "/" ++ img.id ++ ".jpg"

        Nothing ->
            ""


imageEncoder : Image -> Encode.Value
imageEncoder image =
    Encode.object
        [ ( "task_id", Encode.string image.taskId )
        , ( "id", Encode.string image.id )
        ]
