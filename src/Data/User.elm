module Data.User exposing (User, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias User =
    { id : Int
    , name : String
    , isConfirmed : Bool
    }


decoder : Decoder User
decoder =
    decode User
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "isConfirmed" Decode.bool
