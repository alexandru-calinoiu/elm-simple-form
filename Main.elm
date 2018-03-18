module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Validation exposing (..)


type alias Model =
    { email : String
    , message : String
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
    { email = ""
    , message = ""
    , status = NotSubmitted
    }


type Msg
    = InputEmail String
    | InputMessage String
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
            ( { model | email = String.toLower email }, Cmd.none )

        InputMessage message ->
            ( { model | message = message }, Cmd.none )

        Submit ->
            (submitIfValid model)

        SubmitResponse (Ok ()) ->
            ( { model
                | status = Succeeded
                , email = ""
                , message = ""
              }
            , Cmd.none
            )

        SubmitResponse (Err _) ->
            ( { model | status = Failed }, Cmd.none )


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult : Result String (Cmd Msg)
        submissionResult =
            Result.map2
                submit
                (model.email |> isEmail)
                (model.message |> isNotEmpty)
    in
        case submissionResult of
            Ok cmd ->
                ( { model | status = InProgress }, cmd )

            Err _ ->
                ( { model | status = NotValid }, Cmd.none )


submit : String -> String -> Cmd Msg
submit email message =
    let
        url =
            "http://localhost:9292/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "message", Encode.string message )
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


body : Model -> Html Msg
body model =
    div []
        [ div []
            [ input
                [ placeholder "your email"
                , type_ "email"
                , onInput InputEmail
                , value model.email
                , required True
                ]
                []
            ]
        , div []
            [ textarea
                [ placeholder "your message"
                , rows 7
                , onInput InputMessage
                , value model.message
                , required True
                ]
                []
            ]
        ]


footer : Model -> Html Msg
footer model =
    div []
        [ button [ type_ "submit", disabled (model.status == InProgress) ] [ text "Submit" ]
        , button [ type_ "button" ] [ text "Concel" ]
        ]
