module Validation exposing (..)


type alias Validator input output =
    input -> Result input output


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
