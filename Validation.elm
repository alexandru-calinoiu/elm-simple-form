module Validation exposing (..)


type Field field
    = NotValidated String
    | Valid field
    | Invalid String String


type alias Validator a b =
    a -> Result String b


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
(|:) ff fa = apply fa ff

isNotEmpty : Validator String String
isNotEmpty value =
    if value == "" then
        Err "This field is required"
    else
        Ok value


isEmail : Validator String String
isEmail value =
    if String.contains "@" value then
        Ok value
    else
        Err "Please enter a valid email"


isInt : Validator String Int
isInt =
    String.toInt


isPositive : Validator Int Int
isPositive i =
    if i >= 0 then
        Ok i
    else
        Err "I'm expecting a positive integer"


isNatural : Validator String Int
isNatural =
    isInt >=> isPositive
