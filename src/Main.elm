module Main exposing (..)

-- Press a button to send a GET request for random cat GIFs.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, list, map4, string)
import Loading exposing (LoaderType(..), defaultConfig, render)
import Test exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success Image


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGif (Result Http.Error Image)


type alias Image =
    { url : String
    , title : String
    , typeimg : String
    , id : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomCatGif )

        GotGif result ->
            case result of
                Ok baz ->
                    ( Success baz, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Cats" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random cat for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            div []
                [ Loading.render
                    Spinner
                    -- LoaderType
                    { defaultConfig | color = "#333" }
                    -- Config
                    Loading.On

                -- LoadingState
                ]

        Success quux ->
            div []
                [ img [ src quux.url ] []
                , button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , h3 [] [ text quux.title ]
                , h4 [] [ text ("Is un  " ++ quux.typeimg ++ " image format") ]
                , h4 [] [ text ("and its id is : " ++ quux.id) ]
                ]



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
        , expect = Http.expectJson GotGif gifDecoder
        }


gifDecoder : Decoder Image
gifDecoder =
    map4 Image
        (field "data" (field "image_url" string))
        (field "data" (field "title" string))
        (field "data" (field "type" string))
        (field "data" (field "id" string))
