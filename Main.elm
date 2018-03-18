module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Validation exposing (..)


type alias Model =
    { email : Field String
    , message : Field String
    , age : OptionalField Int
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
    { email = NotValidated ""
    , message = NotValidated ""
    , age = NotValidated ""
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
            ( { model | email = NotValidated (String.toLower email) }, Cmd.none )

        InputMessage message ->
            ( { model | message = NotValidated message }, Cmd.none )

        InputAge age ->
            ( { model | age = NotValidated age }, Cmd.none )

        Submit ->
            model |> validateModel |> submitIfValid

        SubmitResponse (Ok ()) ->
            ( { model
                | status = Succeeded
                , email = NotValidated ""
                , message = NotValidated ""
                , age = NotValidated ""
              }
            , Cmd.none
            )

        SubmitResponse (Err _) ->
            ( { model | status = Failed }, Cmd.none )


validateModel : Model -> Model
validateModel model =
    { model
        | email = model.email |> validate (isNotEmpty >=> isEmail)
        , message = model.message |> validate isNotEmpty
        , age = model.age |> validate (optional isNatural)
    }


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult =
            Valid submit
                |: model.email
                |: model.message
                |: model.age
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


extractError : Field a -> Maybe String
extractError value =
    case value of
        Invalid err val ->
            Just err

        _ ->
            Nothing


displayValue : (a -> String) -> Field a -> String
displayValue render field =
    case field of
        Valid val ->
            render val

        Invalid err val ->
            val

        NotValidated val ->
            val


errorLabel : Field a -> Html Msg
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
                , value (model.email |> displayValue identity)
                ]
                []
            , errorLabel model.email
            ]
        , div []
            [ textarea
                [ placeholder "your message *"
                , rows 7
                , onInput InputMessage
                , value (model.message |> displayValue identity)
                ]
                []
            , errorLabel model.message
            ]
        , div []
            [ input
                [ placeholder "your age"
                , type_ "number"
                , onInput InputAge
                , value (model.age |> displayValue (Maybe.map toString >> Maybe.withDefault ""))
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
