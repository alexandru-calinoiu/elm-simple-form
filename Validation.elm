module Validation exposing (..)

import Regex


type Field field
    = NotValidated String
    | Valid field
    | Invalid String String


type alias OptionalField a =
    Field (Maybe a)


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


validate : Validator String a -> Field a -> Field a
validate validator field =
    case field of
        NotValidated value ->
            case validator value of
                Ok a ->
                    Valid a

                Err err ->
                    Invalid err value

        _ ->
            field


apply : Field a -> Field (a -> b) -> Field b
apply fa ff =
    case fa of
        NotValidated s ->
            NotValidated s

        Invalid err s ->
            Invalid err s

        Valid a ->
            case ff of
                NotValidated s ->
                    NotValidated s

                Invalid err s ->
                    Invalid err s

                Valid f ->
                    f a |> Valid


(|:) : Field (a -> b) -> Field a -> Field b
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
