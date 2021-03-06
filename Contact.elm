module Contact exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onSubmit, onInput)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Validation exposing (..)


type alias Model =
    { email : Field String String
    , message : Field String String
    , acceptPolicy : Field Bool Bool
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
    , acceptPolicy = field False
    , status = NotSubmitted
    }


type Msg
    = InputEmail String
    | InputMessage String
    | CheckAcceptPolicy Bool
    | Submit
    | SubmitResponse (Result Http.Error ResponseBody)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email ->
            ( { model | email = field (String.toLower email) }, Cmd.none )

        InputMessage message ->
            ( { model | message = field message }, Cmd.none )

        CheckAcceptPolicy a ->
            ( { model | acceptPolicy = field a }, Cmd.none )

        Submit ->
            model |> validateModel |> submitIfValid

        SubmitResponse (Ok (Ok ())) ->
            ( { initialModel | status = Succeeded }, Cmd.none )

        SubmitResponse (Ok (Err errs)) ->
            ( { model | status = NotSubmitted }
                |> applyServerValidationErrors errs
            , Cmd.none
            )

        SubmitResponse (Err _) ->
            ( { model | status = Failed }, Cmd.none )


applyServerValidationErrors : List ServerValidationError -> Model -> Model
applyServerValidationErrors errs model =
    let
        applyError err m =
            case err of
                EmailAlreadyRegistered ->
                    { m
                        | email =
                            model.email
                                |> setError "This email is bad."
                    }
    in
        errs |> List.foldl applyError model


validateModel : Model -> Model
validateModel model =
    let
        emailValidation =
            isNotEmpty "An email is required" >=> isEmail "Please ensure this is a valid email"

        email =
            model.email |> validate OnSubmit emailValidation

        messageValidation =
            isNotEmpty "An message is required"

        message =
            model.message |> validate OnSubmit messageValidation

        acceptPolicy =
            model.acceptPolicy |> validate OnSubmit (isTrue "You must accept policy")
    in
        { model
            | email = email
            , message = message
            , acceptPolicy = acceptPolicy
        }


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult =
            Valid submit
                |: (validity model.email)
                |: (validity model.message)
                |: (validity model.acceptPolicy)
    in
        case submissionResult of
            Valid cmd ->
                ( { model | status = InProgress }, cmd )

            _ ->
                ( { model | status = NotValid }, Cmd.none )


submit : String -> String -> Bool -> Cmd Msg
submit email message acceptPolicy =
    let
        url =
            "http://localhost:9292/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "message", Encode.string message )
                , ( "acceptPolicy", Encode.bool acceptPolicy )
                ]

        request : Http.Request ResponseBody
        request =
            Http.post url (Http.jsonBody json) decodeResponseBody
    in
        request |> Http.send SubmitResponse


decodeResponseBody : Json.Decoder ResponseBody
decodeResponseBody =
    Json.map2
        (\success errs ->
            if success then
                Ok ()
            else
                Err (errs |> Maybe.withDefault [])
        )
        (Json.field "success" Json.bool)
        (Json.list decodeServerValidationError
            |> Json.field "errors"
            |> Json.maybe
        )


decodeServerValidationError : Json.Decoder ServerValidationError
decodeServerValidationError =
    Json.string
        |> Json.andThen
            (\str ->
                case str of
                    "email_already_registered" ->
                        Json.succeed EmailAlreadyRegistered

                    s ->
                        Json.fail <| "Unexpected value: " ++ s
            )


type alias ResponseBody =
    Result (List ServerValidationError) ()


type ServerValidationError
    = EmailAlreadyRegistered


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
                [ type_ "checkbox"
                , onCheck CheckAcceptPolicy
                , value (model.acceptPolicy |> rawValue |> toString)
                ]
                []
            , label [] [ text "I accept Policy" ]
            ]
        , div [] [ errorLabel model.acceptPolicy ]
        ]


footer : Model -> Html Msg
footer model =
    div []
        [ button [ type_ "submit", disabled (model.status == InProgress) ] [ text "Submit" ]
        , button [ type_ "button" ] [ text "Concel" ]
        ]
