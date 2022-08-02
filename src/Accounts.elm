module Accounts exposing (..)

-- import Time

import Browser
import Dict
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Time


type Msg
    = AccountRes (Result Http.Error String)
    | Tick Time.Posix


type alias Account =
    { auction_count : Int
    , level : Int
    , avatar : Int
    , class : String
    , deaths : Int
    , diamond : Int
    , gold : Int
    , map_index : Int
    , mobs : Int
    , --       pos: {
      --         x: Float,
      --         y: Float,
      --         z: Float
      --       },
      server_id : Int
    , uuid : String
    , online : String
    , process_alive : Bool
    }


decode_account =
    Decode.succeed Account
        |> required "auction_count" Decode.int
        |> optional "level" Decode.int 0
        |> required "avatar" Decode.int
        |> required "class" Decode.string
        |> required "deaths" Decode.int
        |> required "diamond" Decode.int
        |> required "gold" Decode.int
        |> required "map_index" Decode.int
        |> required "mobs" Decode.int
        |> required "server_id" Decode.int
        |> required "uuid" Decode.string
        |> required "online" Decode.string
        |> required "process_alive" Decode.bool


type alias AccountResJson =
    { took : Int
    , accs : List Account
    }


decode_accounts_res =
    Decode.succeed AccountResJson
        |> required "took" Decode.int
        |> required "accs" (Decode.list decode_account)


query_accounts_cmd model =
    let
        uri =
            "http://168.119.74.89:20030/accs_jsx"

        body =
            Encode.encode 0
                (Encode.object
                    [ ( "alive", Encode.bool True )
                    ]
                )
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString AccountRes
        }


type alias Model =
    { accs : Maybe (Dict.Dict String Account)
    , accs_sorted : List Account
    , took : Int
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initial =
            { accs = Nothing
            , accs_sorted = []
            , took = 0
            , error = ""
            }
    in
    ( initial, query_accounts_cmd initial )


subscriptions model =
    Time.every 3000 Tick


update msg model =
    case msg of
        AccountRes sr ->
            ( proc_accounts model False sr, Cmd.none )

        Tick _ ->
            ( model, query_accounts_cmd model )


sort_accs accs =
    List.sortBy .server_id accs


proc_accounts model is_update sr =
    let
        to_map x =
            Dict.fromList (List.map (\acc -> ( acc.uuid, acc )) x)
    in
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_accounts_res fullText of
                Ok login_info ->
                    { model
                        | accs = Just (to_map login_info.accs)
                        , accs_sorted = sort_accs login_info.accs
                        , took = login_info.took
                    }

                Err desc ->
                    let
                        err =
                            Decode.errorToString desc
                    in
                    { model | error = err }

        _ ->
            model


view model =
    let
        accs =
            case model.accs of
                Just vals ->
                    List.map view_acc model.accs_sorted

                Nothing ->
                    []
    in
    div []
        [ div []
            [ text ("took " ++ String.fromInt model.took ++ "ms")
            ]
        , div []
            [ table []
                ([ tr []
                    [ th [] [ text "uuid" ]
                    , th [] [ text "level" ]
                    , th [] [ text "auctions" ]
                    , th [] [ text "avatar" ]
                    , th [] [ text "class" ]
                    , th [] [ text "deaths" ]
                    , th [] [ text "diamond" ]
                    , th [] [ text "gold" ]
                    , th [] [ text "map_index" ]
                    , th [] [ text "mobs" ]
                    , th [] [ text "pos" ]
                    , th [] [ text "server_id" ]
                    , th [] [ text "online" ]
                    , th [] [ text "pid" ]
                    ]
                 ]
                    ++ accs
                )
            ]
        ]


view_acc acc =
    let
        bgcolor =
            if acc.process_alive && acc.online == "ingame" then
                "lightgreen"

            else
                "pink"
    in
    tr [ Html.Attributes.style "background-color" bgcolor ]
        [ td
            []
            [ text acc.uuid ]
        , td [] [ text (String.fromInt acc.level) ]
        , td [] [ text (String.fromInt acc.auction_count) ]
        , td [] [ text (String.fromInt acc.avatar) ]
        , td [] [ text acc.class ]
        , td [] [ text (String.fromInt acc.deaths) ]
        , td [] [ text (String.fromInt acc.diamond) ]
        , td [] [ text (String.fromInt acc.gold) ]
        , td [] [ text (String.fromInt acc.map_index) ]
        , td [] [ text (String.fromInt acc.mobs) ]
        , td [] [ text "" ]
        , td [] [ text (String.fromInt acc.server_id) ]
        , td [] [ text acc.online ]
        , td []
            [ text
                (if acc.process_alive then
                    "yes"

                 else
                    "no"
                )
            ]
        ]
