module OrdersManage exposing (..)

import Browser
import Date
import Debug
import Dict
import Html
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Lang exposing (..)
import List exposing (map)
import SingleDatePicker as DatePicker exposing (TimePickerVisibility, defaultTimePickerSettings)
import Task exposing (Task)
import Time exposing (Month(..), Posix, Zone, here)


type alias Order =
    { uuid : String
    , all_diamonds : Int
    , moved_diamonds : Int
    , money : Float
    , date : String
    , note : String
    , server_id : Int
    , completed : Int
    , canceled : Int
    , failed : Int
    , total : Int
    , price : Float
    , trades : List Trade
    }


type alias NetData =
    { --NetData里面的值
      orders : List Order
    , user : User
    }


type alias User =
    { username : String
    , credits : String
    , allow_credits : String
    }


type NetDataSt
    = NotLoggedIn
    | Loading
    | ND NetData --网络数据的状态


type alias Error =
    { str : String
    }


type alias Trade =
    { details : String
    , diamonds : Int
    , name : String
    , status : String
    , uuid : String
    , count : Int
    }


type alias ServerStock =
    { serverId : Int
    , name : String
    , price : Float
    , total : Int
    , stock : String
    }


type alias History =
    { orders : List Order
    , sum : Float
    }

type alias Session = String

type alias Model =
    { netdata : NetDataSt
    , error : String
    , token : String

    -- input fields
    , username : String
    , password : String
    , order_note : String
    , order_bulk : String
    , order_server : String
    , selected_order : String
    , place_order_error : String
    , placing_order : Bool
    , canceling_trade : Maybe String
    , currentTime : Posix
    , zone : Zone
    , pickedTime : Maybe Posix
    , picker : DatePicker.DatePicker
    , serv_map : Dict.Dict Int String
    , text : Lang.LocalText
    , history_sum : Float
    , servers : Maybe (List ServerStock)
    , viewPage : ViewPage
    , history : Maybe (List Order)
    , session : Session
    }


initial_model : Model
initial_model =
    { netdata = Loading
    , error = ""
    , token = ""
    , username = ""
    , password = ""
    , order_note = ""
    , order_bulk = ""
    , order_server = ""
    , selected_order = ""
    , place_order_error = ""
    , placing_order = False
    , canceling_trade = Nothing
    , currentTime = Time.millisToPosix 0
    , zone = Time.utc
    , pickedTime = Nothing
    , picker = DatePicker.init
    , serv_map = Dict.empty
    , text = Lang.eng_text
    , history_sum = 0.0
    , servers = Nothing
    , viewPage = ViewOrders
    , history = Nothing
    , session = "7B2270617373776F7264223A227869616F20313233222C2275736572223A227869616F687569227D"
    }


type Msg
    = LoginRes (Result Http.Error String)
    | StatusRes (Result Http.Error String)
    | Login
    | Tick Time.Posix
    | AdjustTimeZone Zone
    | SetCurrentTime Posix
    | Change (Model -> String -> Model) String
    | ArchiveClick String
    | SelectOrder String
    | PlaceOrderRes (Result Http.Error String)
    | TradeEditChangeText String
    | CancelTrade String
    | ShowEditTrade Trade
    | ServerListRes (Result Http.Error String)
    | ShowStock
    | ShowOrders
    | QueryHistory
    | ShowHistory
    | HistoryRes (Result Http.Error String)


type alias Status =
    { orders : List Order
    , user : User
    }


type alias LoginInfo =
    { token : String }


type ViewPage
    = ViewServers
    | ViewOrders
    | ViewHistory
    | ViewEditTrade EditTradeInfo


type alias EditTradeInfo =
    { uuid : String
    , text : String
    , error : Maybe String
    }


type alias PlaceRes =
    { result : String
    , details : String
    }


update msg model =
    case msg of
        Change f data ->
            ( f model data, Cmd.none )

        Tick _ ->
            ( model, query_status model.token )

        StatusRes sr ->
            ( update_status model sr, Cmd.none )

        --LoginRes sr -> (login_res model sr, Lang.Session_Data st,Cmd.none)
        LoginRes sr ->
            ( login_res model sr, Cmd.none )

        Login ->
            ( model, login_cmd model )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        SetCurrentTime newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        ArchiveClick order_uuid ->
            ( model, archive_order_cmd model order_uuid )

        SelectOrder order_uuid ->
            if model.selected_order == order_uuid then
                ( { model | selected_order = "" }, Cmd.none )

            else
                ( { model | selected_order = order_uuid }, Cmd.none )

        PlaceOrderRes sr ->
            ( placeOrder_res model sr, Cmd.none )

        CancelTrade uuid ->
            cancel_trade model uuid

        ShowEditTrade tradeInfo ->
            ( show_edit_trade model tradeInfo, Cmd.none )

        ShowStock ->
            ( { model | viewPage = ViewServers }, server_cmd model )

        ShowOrders ->
            ( { model | viewPage = ViewOrders }, Cmd.none )

        QueryHistory ->
            ( { model | viewPage = ViewHistory }, history_cmd model )

        ShowHistory ->
            ( { model | viewPage = ViewHistory }, history_cmd model )

        ServerListRes sr ->
            ( serverListRes model sr, Cmd.none )

        HistoryRes sr ->
            ( update_history model sr, Cmd.none )

        TradeEditChangeText x ->
            case model.viewPage of
                ViewEditTrade tradeInfo ->
                    let
                        nTradeInfo =
                            { tradeInfo | text = x }
                    in
                    ( { model | viewPage = ViewEditTrade nTradeInfo }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view s =
    case (Debug.log "netdata" s.netdata) of
        Loading ->
            Html.div []
                [ Html.text s.error
                , Html.br [] []
                , Html.text "loading"
                ]

        NotLoggedIn ->
            login_form s

        ND netdata ->
            view_orders s netdata.orders s.selected_order


init : String -> ( Model, Cmd Msg )
init session =
    let
        initial =
            initial_model

        ----      task = Task.map2 Time.here Time.now
    in
    ( initial
    , Cmd.batch
        [ Task.perform SetCurrentTime Time.now
        , Task.perform AdjustTimeZone Time.here
        , query_status (Debug.log "session:" session)
        ]
    )


login_form model =
    Html.div []
        [ Html.div [] [ Html.text model.error ]
        , Html.div [] [ Html.text "gotta login" ]
        , Html.div [] [ Html.text "username:", Html.input [ onInput (Change (\s x -> { s | username = x })) ] [ Html.text model.username ] ]
        , Html.div [] [ Html.text "pass:", Html.input [ onInput (Change (\s x -> { s | password = x })) ] [ Html.text model.password ] ]
        , Html.div [] [ Html.button [ onClick Login ] [ Html.text "login" ] ]
        ]



--获取登录的信息


query_status : String -> Cmd Msg
query_status token =
    let
        uri =
            "http://168.119.74.89:30110/jsx_orders"

        body =
            "{'token': '" ++ token ++ "'}"
    in
    Http.post
        { url = uri
        , expect = Http.expectString StatusRes
        , body = Http.stringBody "application/json" body
        }


update_status s sr =
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_error fullText of
                --如果检测出错误就说明登录不成功
                Ok err ->
                    -- { str: "gotta_login"}
                    { s | netdata = NotLoggedIn }

                _ ->
                    case Decode.decodeString decode_status fullText of
                        Ok status ->
                            { s | netdata = ND status }

                        Err desc ->
                            let
                                err =
                                    Decode.errorToString desc
                            in
                            { s | error = err }

        _ ->
            s


decode_error : Decoder Error
decode_error =
    Decode.succeed Error
        |> required "error" Decode.string


login_cmd : Model -> Cmd Msg
login_cmd model =
    let
        uri =
            "http://168.119.74.89:30110/jsx_login"

        body =
            "{'username': '" ++ model.username ++ "', 'password':'" ++ model.password ++ "'}"
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString LoginRes
        }


login_res s sr =
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_login fullText of
                Ok login_info ->
                    { s | token = login_info.token, session = login_info.token }

                Err desc ->
                    let
                        err =
                            Decode.errorToString desc
                    in
                    { s | error = err }

        _ ->
            s


decode_status : Decoder Status
decode_status =
    Decode.succeed Status
        |> required "orders" (Decode.list decode_order)
        |> required "user" decode_user


decode_login : Decoder LoginInfo
decode_login =
    Decode.succeed LoginInfo
        |> required "token" Decode.string


decode_user =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "used_credits" Decode.string
        |> required "allow_credits" Decode.string


decode_order =
    Decode.succeed Order
        |> required "uuid" Decode.string
        |> required "all_diamonds" Decode.int
        |> required "moved_diamonds" Decode.int
        |> required "money" Decode.float
        |> required "date" Decode.string
        |> required "note" Decode.string
        |> required "server_id" Decode.int
        |> required "completed" Decode.int
        |> required "canceled" Decode.int
        |> required "failed" Decode.int
        |> required "total" Decode.int
        |> optional "price" Decode.float 0
        |> required "trades" (Decode.list decode_trade)


decode_trade =
    Decode.succeed Trade
        |> required "details" Decode.string
        |> required "diamonds" Decode.int
        |> required "name" Decode.string
        |> required "status" Decode.string
        |> required "uuid" Decode.string
        |> required "count" Decode.int


view_orders text orders selected_order =
    let
        selected_trades =
            case List.filter (\x -> selected_order == x.uuid) orders of
                [ a ] ->
                    a.trades

                _ ->
                    []
    in
    Html.div [ Html.Attributes.style "display" "block" ]
        [ Html.div
            [ Html.Attributes.style "border" "1px solid"
            , Html.Attributes.style "float" "left"
            ]
            [ Html.table [] (List.concat (map (view_order text selected_order) orders))
            ]
        , Html.div
            [ Html.Attributes.style "border" "1px solid"
            , Html.Attributes.style "float" "left"
            ]
            []
        ]


view_order model selected_order order =
    let
        text =
            model.text

        isSelected =
            case selected_order == order.uuid of
                True ->
                    " <<<< "

                False ->
                    ""

        state =
            order_state order

        server =
            case Dict.get order.server_id model.serv_map of
                Just name ->
                    name

                Nothing ->
                    "unk"

        color_attrib =
            case state of
                Canceled ->
                    [ Html.Attributes.style "background-color" "indianred"
                    ]

                PartialComplete ->
                    [ Html.Attributes.style "background-color" "yellow"
                    ]

                PartialFail ->
                    [ Html.Attributes.style "background-color" "yellow"
                    ]

                Completed ->
                    [ Html.Attributes.style "background-color" "lightgreen"
                    ]

                Queued ->
                    [ Html.Attributes.style "background-color" "#e0ffff"
                    ]

        actions =
            [ --#Html.button [onClick (CancelClick order.uuid)] [Html.text "cancel"]
              Html.button [ onClick (ArchiveClick order.uuid) ] [ Html.text text.archive ]
            ]

        maybe_display =
            (selected_order == order.uuid)
                || (case state of
                        Canceled ->
                            False

                        PartialFail ->
                            True

                        PartialComplete ->
                            False

                        Completed ->
                            False

                        Queued ->
                            True
                   )

        display_trades =
            if maybe_display then
                [ Html.tr []
                    [ Html.td [ Html.Attributes.colspan 9 ]
                        [ Html.table [] (map (view_trade model) order.trades)
                        ]
                    ]
                ]

            else
                []
    in
    [ Html.tr (List.concat [ [ onClick (SelectOrder order.uuid) ], color_attrib ])
        [ Html.td []
            [ Html.text order.date
            ]
        , Html.td []
            [ Html.text <|
                server
                    ++ " ("
                    ++ String.fromInt order.server_id
                    ++ ")"
            ]
        , Html.td []
            [ Html.text order.uuid
            ]
        , Html.td []
            [ Html.text (String.slice 0 50 order.note)
            ]
        , Html.td []
            [ Html.text <| String.fromInt order.moved_diamonds
            ]
        , Html.td []
            [ Html.text <| String.fromInt order.all_diamonds
            ]
        , Html.td []
            [ Html.text <| String.fromFloat order.price
            ]
        , Html.td []
            [ Html.text <| String.fromFloat order.money
            ]
        , Html.td []
            [ Html.text <|
                (String.fromInt order.completed
                    ++ " / "
                    ++ String.fromInt order.total
                    ++ " ("
                    ++ String.fromInt order.canceled
                    ++ ")"
                )
            ]
        , Html.td [] actions
        , Html.td []
            [ Html.text isSelected
            ]
        ]
    ]
        ++ display_trades


type OrderState
    = Canceled
    | PartialFail
    | Completed
    | Queued
    | PartialComplete


order_state order =
    if order.canceled == order.total then
        Canceled

    else if (order.completed == order.total) && (order.canceled > 0) then
        PartialComplete

    else if order.completed == order.total then
        Completed

    else if order.failed > 0 then
        PartialFail

    else
        Queued


archive_order_cmd model order_uuid =
    let
        uri =
            "http://168.119.74.89:30110/archive_order"

        body_json =
            Encode.object
                [ ( "order_uuid", Encode.string order_uuid )
                , ( "token", Encode.string model.token )
                ]

        body =
            Encode.encode 0 body_json
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString PlaceOrderRes
        }


view_trade model trade =
    let
        text =
            model.text

        details =
            case trade.details of
                "duplicate item found" ->
                    text.duplicate_item_found

                "item not found" ->
                    text.item_not_found

                _ ->
                    trade.details

        status =
            case trade.status of
                "Done" ->
                    text.done

                "Queued" ->
                    text.queued

                "Error" ->
                    text.error

                _ ->
                    trade.status

        cancelConfirm =
            case model.canceling_trade == Just trade.uuid of
                True ->
                    "red"

                False ->
                    ""

        actions =
            case trade.status of
                "Error" ->
                    [ Html.button
                        [ onClick (ShowEditTrade trade)
                        ]
                        [ Html.text "edit trade" ]
                    , Html.button
                        [ Html.Attributes.style "background-color" cancelConfirm
                        , onClick (CancelTrade trade.uuid)
                        ]
                        [ Html.text "cancel" ]
                    ]

                "Queued" ->
                    [ Html.button
                        [ Html.Attributes.style "background-color" cancelConfirm
                        , onClick (CancelTrade trade.uuid)
                        ]
                        [ Html.text "cancel" ]
                    ]

                _ ->
                    []
    in
    Html.tr []
        [ Html.td []
            [ Html.text trade.uuid
            ]
        , Html.td []
            [ Html.text <| String.fromInt trade.diamonds
            ]
        , Html.td []
            [ Html.text (trade.name ++ "(" ++ String.fromInt trade.count ++ ")")
            ]
        , Html.td []
            [ Html.text status
            ]
        , Html.td []
            [ Html.text details
            ]
        , Html.td [] actions
        ]


placeOrder_res old_s sr =
    let
        s =
            { old_s | placing_order = False }
    in
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_placeOrder_res fullText of
                Ok res ->
                    if res.result == "ok" then
                        { s | place_order_error = res.details, order_bulk = "" }

                    else
                        { s | place_order_error = res.details }

                Err desc ->
                    let
                        err =
                            Decode.errorToString desc
                    in
                    { s | error = err }

        _ ->
            s


cancel_trade_cmd : Model -> String -> Cmd Msg
cancel_trade_cmd model uuid =
    let
        uri =
            "http://168.119.74.89:30110/cancel_trade"

        body_json =
            Encode.object
                [ ( "token", Encode.string model.token )
                , ( "trade_uuid", Encode.string uuid )
                ]

        body =
            Encode.encode 0 body_json
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString ServerListRes
        }


cancel_trade model uuid =
    if model.canceling_trade == Just uuid then
        ( { model | canceling_trade = Nothing }, cancel_trade_cmd model uuid )

    else
        ( { model | canceling_trade = Just uuid }, Cmd.none )


show_edit_trade s trade =
    let
        tradeInfo =
            { uuid = trade.uuid
            , text = trade_text
            , error = Nothing
            }

        trade_text =
            String.fromInt trade.diamonds ++ " " ++ trade.name ++ " " ++ String.fromInt trade.count
    in
    { s | viewPage = ViewEditTrade tradeInfo }


decode_placeOrder_res =
    Decode.succeed PlaceRes
        |> required "result" Decode.string
        |> optional "details" Decode.string ""


serverListRes old_s sr =
    let
        s =
            { old_s | placing_order = False }
    in
    case sr of
        Ok fullText ->
            case Decode.decodeString (Decode.list decode_server_res) fullText of
                Ok res ->
                    { s | servers = Just res, serv_map = serv_map res }

                Err desc ->
                    let
                        err =
                            Decode.errorToString desc
                    in
                    { s | error = err }

        _ ->
            s


serv_map : List ServerStock -> Dict.Dict Int String
serv_map list =
    Dict.fromList (List.map (\x -> ( x.serverId, x.name )) list)


decode_server_res =
    Decode.succeed ServerStock
        |> required "serverId" Decode.int
        |> required "name" Decode.string
        |> required "price" Decode.float
        |> required "total" Decode.int
        |> required "stock" Decode.string


history_cmd : Model -> Cmd Msg
history_cmd model =
    let
        time =
            case model.pickedTime of
                Just x ->
                    x

                Nothing ->
                    model.currentTime

        date =
            Date.fromPosix model.zone time

        uri =
            "http://168.119.74.89:30110/history"

        body_json =
            Encode.object
                [ ( "token", Encode.string model.token )
                , ( "start_m", Encode.int (Date.month date |> Date.monthToNumber) )
                , ( "start_d", Encode.int (Date.day date) )
                , ( "start_y", Encode.int (Date.year date) )
                ]

        body =
            Encode.encode 0 body_json
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString HistoryRes
        }


server_cmd : Model -> Cmd Msg
server_cmd model =
    let
        uri =
            "http://168.119.74.89:30110/server_list_jsx"

        body_json =
            Encode.object
                [ ( "token", Encode.string model.token )
                ]

        body =
            Encode.encode 0 body_json
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString ServerListRes
        }


update_history s sr =
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_error fullText of
                Ok err ->
                    -- { str: "gotta_login"}
                    { s | netdata = NotLoggedIn }

                _ ->
                    case Decode.decodeString decode_history fullText of
                        Ok status ->
                            { s | history = Just status.orders, history_sum = status.sum }

                        Err desc ->
                            let
                                err =
                                    Decode.errorToString desc
                            in
                            { s | error = err }

        _ ->
            s


decode_history : Decoder History
decode_history =
    Decode.succeed History
        |> required "orders" (Decode.list decode_order)
        |> required "sum" Decode.float
