module Account exposing (..)


type alias AccountModel =
    { uuid : String, log : List ( Int, String ) }
