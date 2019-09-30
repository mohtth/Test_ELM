module Test exposing (..)

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
import Json.Decode exposing (Decoder, field, list, map5, string)
import Loading exposing (LoaderType(..), defaultConfig, render)



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
    | Success Joke


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRdmJoke )



-- UPDATE


type Msg
    = MorePlease
    | GotJoke (Result Http.Error Joke)


type alias Joke =
    { category : String
    , typejoke : String
    , joke : String
    , delivery : String
    , id : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRdmJoke )

        GotJoke result ->
            case result of
                Ok happy ->
                    ( Success happy, Cmd.none )

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
        [ h2 [] [ text "Random Jokes" ]
        , viewJoke model
        ]


viewJoke : Model -> Html Msg
viewJoke model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random joke for some reason. "
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

        -- il existe deux genre de blagues, "types" -> single/twopart
        -- il faut creer un "if"
        Success happy ->
            div []
                [ h3 [] [ text happy.category ]
                , button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , h4 [] [ text ("Is un : " ++ happy.typejoke) ]
                , h4 [] [ text ("My joke : " ++ happy.joke) ]
                , h3 [] [ text happy.delivery ]
                ]



-- HTTP


getRdmJoke : Cmd Msg
getRdmJoke =
    Http.get
        { url = "https://sv443.net/jokeapi/category/Programming"
        , expect = Http.expectJson GotJoke jokeDecoder
        }


jokeDecoder : Decoder Joke
jokeDecoder =
    map5 Joke
        (field "category" string)
        (field "type" string)
        (field "joke" string)
        (field "delivery" string)
        (field "id" string)
