module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Validation exposing (..)


type alias Model =
    { email : Field String String
    , message : Field String String
    , age : OptionalField String Int
    , status : SubmissionStatus
    }


type SubmissionStatus
    = NotSubmitted
    | NotValid
    | InProgress
    | Succeeded
    | Failed


initialModel : Model
initialModel =
    { email = field ""
    , message = field ""
    , age = field ""
    , status = NotSubmitted
    }


type Msg
    = InputEmail String
    | InputMessage String
    | InputAge String
    | Submit
    | SubmitResponse (Result Http.Error ())


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email ->
            ( { model | email = field (String.toLower email) }, Cmd.none )

        InputMessage message ->
            ( { model | message = field message }, Cmd.none )

        InputAge age ->
            ( { model | age = field age }, Cmd.none )

        Submit ->
            model |> validateModel |> submitIfValid

        SubmitResponse (Ok ()) ->
            ( { initialModel | status = Succeeded }
            , Cmd.none
            )

        SubmitResponse (Err _) ->
            ( { model | status = Failed }, Cmd.none )


validateModel : Model -> Model
validateModel model =
    let
        emailValidation =
            isNotEmpty "An email is required" >=> isEmail "Please ensure this is a valid email"

        email =
            model.email |> validate emailValidation

        messageValidation =
            isNotEmpty "An message is required"

        message =
            model.message |> validate messageValidation

        ageValidation =
            optional (isNatural "Age should be a natural number")

        age =
            model.age |> validate ageValidation
    in
        { model
            | email = email
            , message = message
            , age = age
        }


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult =
            Valid submit
                |: (validity model.email)
                |: (validity model.message)
                |: (validity model.age)
    in
        case submissionResult of
            Valid cmd ->
                ( { model | status = InProgress }, cmd )

            _ ->
                ( { model | status = NotValid }, Cmd.none )


submit : String -> String -> Maybe Int -> Cmd Msg
submit email message age =
    let
        url =
            "http://localhost:9292/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "message", Encode.string message )
                , ( "age"
                  , age
                        |> Maybe.map Encode.int
                        |> Maybe.withDefault Encode.null
                  )
                ]

        decoder =
            Decode.succeed ()

        request : Http.Request ()
        request =
            Http.post url (Http.jsonBody json) decoder
    in
        request |> Http.send SubmitResponse


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit Submit
        , novalidate True
        ]
        [ header model
        , body model
        , footer model
        , div [] [ model |> toString |> text ]
        ]


header : Model -> Html msg
header model =
    div []
        [ h1 [] [ text "Contact Us" ]
        , renderStatus model.status
        ]


renderStatus : SubmissionStatus -> Html msg
renderStatus status =
    case status of
        NotSubmitted ->
            div [] []

        NotValid ->
            div [] [ text "Not vaild" ]

        InProgress ->
            div [] [ text "Your request is being submitted" ]

        Succeeded ->
            div [] [ text "Your request was ok" ]

        Failed ->
            div [] [ text "Your request failed" ]


extractError : Field raw a -> Maybe String
extractError field =
    case validity field of
        Invalid err ->
            Just err

        _ ->
            Nothing


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
                [ placeholder "your email *"
                , type_ "email"
                , onInput InputEmail
                , value (model.email |> rawValue)
                ]
                []
            , errorLabel model.email
            ]
        , div []
            [ textarea
                [ placeholder "your message *"
                , rows 7
                , onInput InputMessage
                , value (model.message |> rawValue)
                ]
                []
            , errorLabel model.message
            ]
        , div []
            [ input
                [ placeholder "your age"
                , onInput InputAge
                , value (model.age |> rawValue)
                ]
                []
            , errorLabel model.age
            ]
        ]


footer : Model -> Html Msg
footer model =
    div []
        [ button [ type_ "submit", disabled (model.status == InProgress) ] [ text "Submit" ]
        , button [ type_ "button" ] [ text "Concel" ]
        ]
