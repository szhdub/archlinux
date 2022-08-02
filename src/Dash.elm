module Dash exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Time


type alias Model =
    { servers : List Server
    , uuid : String
    , ip : String
    , took : Int
    , error : String
    }


type Msg
    = AccountRes (Result Http.Error String)
    | Tick Time.Posix


init : Model
init =
    { servers = []
    , uuid = ""
    , ip = ""
    , took = 1
    , error = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model, query_accounts_cmd model )

        AccountRes sr ->
            ( proc_accounts model False sr, Cmd.none )



-- _ ->
--    (model, Cmd.none)


proc_accounts model is_update sr =
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_accounts_res fullText of
                Ok response ->
                    { model
                        | servers = response.servers
                        , took = response.took
                        , error = ""
                    }

                Err desc ->
                    let
                        err =
                            Decode.errorToString desc
                    in
                    { model | error = err }

        _ ->
            { model | error = "network error" }


view model =
    div []
        [ div []
            [ text "took"
            , text model.error
            , text " "
            , text (String.fromInt model.took)
            ]
        , div []
            [ div [ Html.Attributes.style "float" "left" ]
                [ table []
                    [ tr []
                        [ th []
                            [ text "item"
                            ]
                        , th []
                            [ text "value"
                            ]
                        ]
                    ]
                ]
            , div [ Html.Attributes.style "float" "left" ]
                [ table [ Html.Attributes.style "border" "1px solid" ]
                    ([ tr []
                        [ th []
                            [ text "server"
                            ]
                        , th []
                            [ text "open"
                            ]
                        , th []
                            [ text "accounts"
                            ]
                        , th []
                            [ text "farmers"
                            ]
                        , th []
                            [ text "makers"
                            ]
                        , th []
                            [ text "diamonds"
                            ]
                        ]
                     ]
                        ++ view_mappings model
                    )
                ]
            ]
        ]


view_mappings model =
    List.map viewServer model.servers


viewServer : Server -> Html Msg
viewServer server =
    let
        is_open =
            if server.open then
                "open"

            else
                ""
    in
    tr []
        [ td []
            [ text (String.fromInt server.server_id)
            ]
        , td []
            [ text is_open
            ]
        , td []
            [ text (String.fromInt server.count)
            ]
        , td []
            [ text (String.fromInt server.farmers)
            ]
        , td []
            [ text (String.fromInt server.char_maker)
            ]
        , td []
            [ text (String.fromInt server.diamonds)
            ]
        , td []
            [ text server.states
            ]
        ]


type alias Server =
    { char_maker : Int
    , count : Int
    , diamonds : Int
    , farmers : Int
    , open : Bool
    , server_id : Int
    , states : String
    }


type alias AccountCountJson =
    { took : Int
    , servers : List Server
    }


decode_accounts_res =
    Decode.succeed AccountCountJson
        |> required "took" Decode.int
        |> required "servers" (Decode.list decode_server)


decode_server =
    Decode.succeed Server
        |> required "char_maker" Decode.int
        |> required "count" Decode.int
        |> required "diamonds" Decode.int
        |> required "farmers" Decode.int
        |> required "open" Decode.bool
        |> required "server_id" Decode.int
        |> optional "states1" Decode.string ""


query_accounts_cmd model =
    let
        uri =
            "http://168.119.74.89:20030/acc_count_jsx"

        body =
            Encode.encode 0
                (Encode.object
                    []
                )
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString AccountRes
        }


type KV
    = List ( String, T )


type T
    = M (List KV)
    | TL (List T)
    | S String
    | I Int
    | B Bool
