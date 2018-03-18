module Register exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
import Validation exposing (..)


type alias Model =
    { email : Field String String
    , password : Field String String
    , confirmPassword : Field String String
    }


initialModel : Model
initialModel =
    { email = field ""
    , password = field ""
    , confirmPassword = field ""
    }


type Msg
    = InputEmail String
    | InputPassword String
    | InputConfirmPassword String
    | Submit

emailValidation : Validator String String
emailValidation =
    isNotEmpty "An email is required" >=> isEmail "Please ensure this is a valid email"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email ->
            ( { model
                | email =
                    model.email
                        |> validate (OnChange email) emailValidation
              }
            , Cmd.none
            )

        InputPassword password ->
            ( { model | password = field password }, Cmd.none )

        InputConfirmPassword confirmPassword ->
            ( { model | confirmPassword = field confirmPassword }, Cmd.none )

        Submit ->
            ( model |> validateModel, Cmd.none )


validateModel : Model -> Model
validateModel model =
    let
        email =
            model.email |> validate OnSubmit emailValidation

        passwordValidation =
            isNotEmpty "Please fill in a password" >=> isStrongPassword "Must be strong"

        password =
            model.password |> validate OnSubmit passwordValidation

        confirmPasswordValidation =
            isNotEmpty "Please retype this password"
                >=> isEqualTo password "The passwords do not match"

        confirmPassword =
            model.confirmPassword |> validate OnSubmit confirmPasswordValidation
    in
        { model
            | email = email
            , password = password
            , confirmPassword = confirmPassword
        }


isStrongPassword : String -> Validator String String
isStrongPassword err s =
    if String.length s >= 6 then
        Ok s
    else
        Err err


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit Submit
        , novalidate True
        ]
        [ header model
        , body model
        , footer model
        ]


header : Model -> Html Msg
header model =
    div []
        [ h1 [] [ text "Register" ] ]


errorLabel : Field raw a -> Html Msg
errorLabel field =
    label
        [ class "label lable-error" ]
        [ field |> extractError |> Maybe.withDefault "" |> text ]


body : Model -> Html Msg
body model =
    div []
        [ div []
            [ input
                [ placeholder "Your email *"
                , type_ "email"
                , onInput InputEmail
                , value (model.email |> rawValue)
                ]
                []
            , errorLabel model.email
            ]
        , div []
            [ input
                [ placeholder "Your password *"
                , type_ "password"
                , onInput InputPassword
                , value (model.password |> rawValue)
                ]
                []
            , errorLabel model.password
            ]
        , div []
            [ input
                [ placeholder "Confirm password *"
                , type_ "password"
                , onInput InputConfirmPassword
                , value (model.confirmPassword |> rawValue)
                ]
                []
            , errorLabel model.confirmPassword
            ]
        ]


footer : Model -> Html Msg
footer model =
    div []
        [ button [ type_ "submit" ] [ text "Submit" ] ]
