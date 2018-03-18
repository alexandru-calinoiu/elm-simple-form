module Validation exposing (..)

import Regex


type Field raw a
    = Field raw (Validity a)


field value =
    Field value NotValidated


rawValue (Field rawValue _) =
    rawValue


validity (Field _ validity) =
    validity


type Validity a
    = NotValidated
    | Valid a
    | Invalid String


type alias OptionalField raw a =
    Field raw (Maybe a)


type alias Validator a b =
    a -> Result String b


optional : Validator String a -> Validator String (Maybe a)
optional validate s =
    if s == "" then
        Ok Nothing
    else
        validate s |> Result.map Just


(>=>) : Validator a b -> Validator b c -> Validator a c
(>=>) f g a =
    case a |> f of
        Ok b ->
            b |> g

        Err s ->
            Err s


validate : Validator raw a -> Field raw a -> Field raw a
validate validate (Field value validity) =
    Field value (validate value |> toValidity)

toValidity : Result String a -> Validity a
toValidity result =
    case result of
        Ok a ->
            Valid a

        Err err ->
            Invalid err


apply : Validity a -> Validity (a -> b) -> Validity b
apply fa ff =
    case fa of
        NotValidated ->
            NotValidated

        Invalid err ->
            Invalid err

        Valid a ->
            case ff of
                NotValidated ->
                    NotValidated

                Invalid err ->
                    Invalid err

                Valid f ->
                    f a |> Valid


(|:) : Validity (a -> b) -> Validity a -> Validity b
(|:) ff fa =
    apply fa ff


type alias ErrorMessage =
    String


isNotEmpty : ErrorMessage -> Validator String String
isNotEmpty err value =
    if value == "" then
        Err err
    else
        Ok value


isEmail : ErrorMessage -> Validator String String
isEmail err value =
    let
        regex =
            Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                |> Regex.caseInsensitive
    in
        if Regex.contains regex value then
            Ok value
        else
            Err err


isInt : ErrorMessage -> Validator String Int
isInt err =
    String.toInt >> (Result.mapError (always err))


isPositive : ErrorMessage -> Validator Int Int
isPositive err i =
    if i >= 0 then
        Ok i
    else
        Err err


isNatural : ErrorMessage -> Validator String Int
isNatural err =
    isInt err >=> isPositive err

isTrue : ErrorMessage -> Validator Bool Bool
isTrue err value =
    if value then
        Ok value
    else
        Err err