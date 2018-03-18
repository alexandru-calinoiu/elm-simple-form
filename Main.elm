module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { email : String
    , message : String
    , status : SubmissionStatus
    }


type SubmissionStatus
    = NotSubmitted
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
            ( { model
                | status = InProgress
                , email = ""
                , message = ""
              }
            , submit model
            )

        SubmitResponse (Ok ()) ->
            ( { model | status = Succeeded }, Cmd.none )

        SubmitResponse (Err _) ->
            ( { model | status = Failed }, Cmd.none )


submit : Model -> Cmd Msg
submit model =
    let
        url =
            "http://localhost:9292/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string model.email )
                , ( "message", Encode.string model.message )
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
        [ onSubmit Submit ]
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
