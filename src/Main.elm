import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : Int
  , submit : Bool
  }


init : Model
init =
  Model "" "" "" 0 False



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name, submit = False }

    Password password ->
      { model | password = password, submit = False }

    PasswordAgain password ->
      { model | passwordAgain = password, submit = False }

    Age age ->
      if String.length age > 0 then
        case String.toInt age of
          Just ageInt ->
            { model | age = ageInt, submit = False }

          Nothing ->
            model
      else
        { model | age = 0, submit = False }

    Submit ->
      { model | submit = True }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "age" "Age" (String.fromInt model.age) Age
    , viewValidation model
    , button [ onClick Submit ] [ text "Submit" ]
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidateModel : Model -> Bool
viewValidateModel model =
  model.password == model.passwordAgain
    && String.length model.password > 8
    && String.any Char.isDigit model.password
    && String.any Char.isUpper model.password
    && String.any Char.isLower model.password
    && model.age > 0

viewValidation : Model -> Html msg
viewValidation model =
  if model.submit && viewValidateModel model
    then
      div [ style "color" "green" ] [ text "OK" ]
  else if model.submit && viewValidateModel model == False
    then
      div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
    div [ style "color" "black" ] [ text "Enter your informations" ]