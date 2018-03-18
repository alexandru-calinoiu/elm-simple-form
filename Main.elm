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
    , submitting : Bool
    }


initialModel : Model
initialModel =
    { email = ""
    , message = ""
    , submitting = False
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
            ( { model | submitting = True }, submit model )

        SubmitResponse result ->
            let _ = Debug.log "result" result
            in (model, Cmd.none)


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

        decoder = Decode.succeed ()

        request : Http.Request ()
        request =
            Http.post url (Http.jsonBody json) decoder  
    in
        request |> Http.send SubmitResponse


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit Submit ]
        [ header
        , body model
        , footer
        , div [] [ model |> toString |> text ]
        ]


header : Html msg
header =
    div []
        [ h1 [] [ text "Contact Us" ] ]


body : Model -> Html Msg
body model =
    div []
        [ div []
            [ input
                [ placeholder "your email"
                , type_ "email"
                , onInput InputEmail
                , value model.email
                ]
                []
            ]
        , div []
            [ textarea
                [ placeholder "your message"
                , rows 7
                , onInput InputMessage
                ]
                []
            ]
        ]


footer : Html Msg
footer =
    div []
        [ button [ type_ "submit" ] [ text "Submit" ]
        , button [ type_ "button" ] [ text "Concel" ]
        ]
