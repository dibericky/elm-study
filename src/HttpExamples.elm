module HttpExamples exposing (Model, main)

{- Ref: https://elmprogramming.com/decoding-json-part-1.html -}

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nicknames = [], errorMessage = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { nicknames : List String
    , errorMessage : Maybe String
    }



-- Action


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List String))


url : String
url =
    "http://localhost:5019/nicknames"


nicknamesDecoder : Decoder (List String)
nicknamesDecoder =
    list string


getNickNames : Cmd Msg
getNickNames =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived nicknamesDecoder
        }



-- Update


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNickNames )

        DataReceived (Ok nicknames) ->
            ( { model | nicknames = nicknames }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewNicknamesOrError model
        ]


viewNicknamesOrError : Model -> Html Msg
viewNicknamesOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewNicknames model.nicknames


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch nicknames at this time"
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewNicknames : List String -> Html Msg
viewNicknames nicknames =
    div []
        [ h3 [] [ text "Old School Main Characters" ]
        , ul [] (List.map viewNickname nicknames)
        ]


viewNickname : String -> Html Msg
viewNickname nickname =
    li [] [ text nickname ]
