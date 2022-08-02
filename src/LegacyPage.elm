module LegacyPage exposing (..)

import Browser
import Date
import Dict
import Html
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List exposing (map)
import SingleDatePicker as DatePicker exposing (TimePickerVisibility, defaultTimePickerSettings)
import Task exposing (Task)
import Time exposing (Month(..), Posix, Zone, here)



-- todo: disable place order button on click until result comes
-- todo: admin panel(add/disable user) (add credit) (change pass) (change server price)
-- todo: add cancel button, archive button, history of trades
-- add price * diamonds of completed orders
-- load prices and server names, show when selecting server


type alias LocalText =
    { archive : String
    , balance : String
    , done : String
    , duplicate_item_found : String
    , history : String
    , item_not_found : String
    , orders : String
    , order_number : String
    , place_order : String
    , queued : String
    , server_id : String
    , stock : String
    , trades : String
    , username : String
    , loading : String
    , price : String
    , error : String
    }


eng_text =
    { queued = "排队登录"
    , done = "完成"
    , history = "隐藏的订单"
    , stock = "库存列表"
    , username = "用户名"
    , balance = "交易金额"
    , duplicate_item_found = "物品价格重复"
    , item_not_found = "无法找到物品"
    , orders = "交易购买"
    , order_number = "订单号"
    , trades = "交易"
    , server_id = "服务器ID"
    , place_order = "确定购买"
    , archive = "隐藏"
    , loading = "loading"
    , price = "price"
    , error = "error"
    }


zh_text =
    { queued = "queued"
    , done = "done"
    , history = "history"
    , stock = "stock"
    , username = "username"
    , balance = "balance"
    , duplicate_item_found = "duplicate item found"
    , item_not_found = "item not found"
    , orders = "orders"
    , order_number = "order number"
    , trades = "trades"
    , server_id = "server id"
    , place_order = "place order"
    , archive = "archive"
    , loading = "loading"
    , price = "price"
    , error = "Error"
    }


type Msg
    = LoginRes (Result Http.Error String)
    | StatusRes (Result Http.Error String)
    | Login
    | Tick Time.Posix
    | Change (Model -> String -> Model) String
    | SelectOrder String
    | PlaceOrder
    | PlaceOrderRes (Result Http.Error String)
    | ArchiveClick String
    | CancelClick String
    | ServerListRes (Result Http.Error String)
    | ShowStock
    | ShowOrders
    | QueryHistory
    | ShowHistory
    | ShowEditTrade Trade
    | HistoryRes (Result Http.Error String)
    | CancelTrade String
    | ResetTradeRes (Result Http.Error String)
    | TradeEditChangeText String
    | ResetTrade
    | OpenPicker
    | UpdatePicker ( DatePicker.DatePicker, Maybe Posix )
    | AdjustTimeZone Zone
    | SetCurrentTime Posix



-- | UsersManageMsg UsersManage.Msg
--| SetView NewView
--type NewView =  UsersManageView


view_error text error =
    case error of
        "" ->
            Html.tr [] []

        _ ->
            Html.tr []
                [ Html.td [] [ Html.text "error: ", Html.text error ]
                ]


view_details text details =
    Html.div []
        [ Html.table []
            [ Html.tr []
                [ {-
                     Html.td [] [
                       Html.button [onClick ShowOrders] [Html.text text.orders]
                       , Html.button [onClick ShowStock ] [Html.text text.stock]
                       , Html.button [onClick ShowHistory] [Html.text text.history]
                       , Html.button [onClick ShowHistory] [Html.text "用户管理"]
                        ]

                  -}
                  Html.td []
                    [ Html.text text.username
                    , Html.text ": "
                    , Html.text details.username
                    ]
                , Html.td []
                    [ Html.text (text.balance ++ ": ")
                    , Html.text details.credits
                    , Html.text " / "
                    , Html.text details.allow_credits
                    ]
                ]
            ]
        ]


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


view_input_order model =
    let
        last_error =
            model.place_order_error
    in
    Html.div []
        [ Html.div []
            [ Html.text last_error
            ]
        , Html.div []
            [ Html.text model.text.order_number
            , Html.input
                [ Html.Attributes.placeholder "optional comment"
                , onInput (Change (\s x -> { s | order_note = x }))
                ]
                [ Html.text model.order_note ]
            ]
        , Html.div []
            [ Html.text model.text.trades
            , Html.textarea
                [ Html.Attributes.cols 60
                , Html.Attributes.rows 5
                , Html.Attributes.placeholder "diamonds itemname1 count(for stakable items)\n509 魔力精髓 1\n501 銀色印記 1"
                , onInput (Change (\s x -> { s | order_bulk = x }))
                ]
                [ Html.text model.order_bulk ]
            ]
        , Html.div []
            [ Html.text model.text.server_id
            , Html.input
                [ onInput (Change (\s x -> { s | order_server = x }))
                ]
                [ Html.text model.order_server ]
            ]
        , if model.placing_order then
            Html.text "placing order"

          else
            Html.button [ onClick PlaceOrder ] [ Html.text model.text.place_order ]
        ]


view_history model =
    let
        select_time =
            case model.pickedTime of
                Just x ->
                    "change time (" ++ Date.format "MMM d, yyyy" (Date.fromPosix model.zone x) ++ ")"

                Nothing ->
                    "change time (" ++ Date.format "MMM d, yyyy" (Date.fromPosix model.zone model.currentTime) ++ ")"
    in
    case model.history of
        Nothing ->
            Html.div [] [ Html.text model.text.loading ]

        Just history ->
            Html.div []
                [ Html.div []
                    [ Html.div []
                        [ Html.button [ onClick OpenPicker ] [ Html.text select_time ]
                        , DatePicker.view (DatePicker.defaultSettings Time.utc UpdatePicker) model.picker
                        ]
                    , Html.div []
                        [ Html.button [ onClick QueryHistory ] [ Html.text "refresh" ]
                        ]
                    , Html.div []
                        [ Html.text ("sum: " ++ String.fromFloat model.history_sum)
                        ]
                    ]
                , Html.div []
                    [ Html.table [] (List.concat (map (view_order model model.selected_order) history))
                    ]
                ]


view_servers model =
    case model.servers of
        Nothing ->
            Html.div [] [ Html.text model.text.loading ]

        Just x ->
            Html.div []
                [ Html.table []
                    ([ Html.tr []
                        [ Html.th
                            []
                            [ Html.text model.text.loading
                            ]
                        , Html.th []
                            [ Html.text model.text.price
                            ]
                        , Html.th []
                            [ Html.text model.text.stock
                            ]
                        ]
                     ]
                        ++ List.map view_server x
                    )
                ]


view_server serv =
    Html.tr [ Html.Attributes.style "border" "1px solid" ]
        [ Html.td
            [ Html.Attributes.style "width" "120px"
            ]
            [ Html.text (String.fromInt serv.serverId ++ " (" ++ serv.name ++ ")")
            ]
        , Html.td
            [ Html.Attributes.style "border" "1px solid"
            ]
            [ Html.text (String.fromFloat serv.price)
            ]
        , Html.td
            [ Html.Attributes.style "border" "1px solid"
            ]
            [ Html.text serv.stock
            ]
        ]


type alias ServerStock =
    { serverId : Int
    , name : String
    , price : Float
    , total : Int
    , stock : String
    }


type alias Trade =
    { details : String
    , diamonds : Int
    , name : String
    , status : String
    , uuid : String
    , count : Int
    }


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


type alias Status =
    { orders : List Order
    , user : User
    }


type alias User =
    { username : String
    , credits : String
    , allow_credits : String
    }


type alias Error =
    { str : String
    }


type alias PlaceRes =
    { result : String
    , details : String
    }


decode_placeOrder_res =
    Decode.succeed PlaceRes
        |> required "result" Decode.string
        |> optional "details" Decode.string ""


decode_error : Decoder Error
decode_error =
    Decode.succeed Error
        |> required "error" Decode.string


decode_status : Decoder Status
decode_status =
    Decode.succeed Status
        |> required "orders" (Decode.list decode_order)
        |> required "user" decode_user


type alias History =
    { orders : List Order
    , sum : Float
    }


decode_history : Decoder History
decode_history =
    Decode.succeed History
        |> required "orders" (Decode.list decode_order)
        |> required "sum" Decode.float


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


decode_user =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "used_credits" Decode.string
        |> required "allow_credits" Decode.string


type alias LoginInfo =
    { token : String }


decode_login : Decoder LoginInfo
decode_login =
    Decode.succeed LoginInfo
        |> required "token" Decode.string


decode_server_res =
    Decode.succeed ServerStock
        |> required "serverId" Decode.int
        |> required "name" Decode.string
        |> required "price" Decode.float
        |> required "total" Decode.int
        |> required "stock" Decode.string


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


encode_order : String -> String -> String -> String -> Encode.Value
encode_order server bulk note token =
    Encode.object
        [ ( "server", Encode.string server )
        , ( "bulk", Encode.string bulk )
        , ( "note", Encode.string note )
        , ( "token", Encode.string token )
        ]


place_order_cmd model =
    let
        uri =
            "http://168.119.74.89:30110/place_order"

        body =
            Encode.encode 0
                (encode_order
                    model.order_server
                    model.order_bulk
                    model.order_note
                    model.token
                )
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString PlaceOrderRes
        }


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


reset_trade_cmd model tradeInfo =
    let
        uri =
            "http://168.119.74.89:30110/reset_trade"

        body_json =
            Encode.object
                [ ( "token", Encode.string model.token )
                , ( "trade_uuid", Encode.string tradeInfo.uuid )
                , ( "content", Encode.string tradeInfo.text )
                ]

        body =
            Encode.encode 0 body_json
    in
    Http.post
        { url = uri
        , body = Http.stringBody "application/json" body
        , expect = Http.expectString ResetTradeRes
        }


type alias NetData =
    { orders : List Order
    , user : User
    }


type alias EditTradeInfo =
    { uuid : String
    , text : String
    , error : Maybe String
    }


type ViewPage
    = ViewServers
    | ViewOrders
    | ViewHistory
    | ViewEditTrade EditTradeInfo


type NetDataSt
    = NotLoggedIn
    | Loading
    | ND NetData


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
    , text : LocalText

    --
    , selected_order : String
    , place_order_error : String
    , placing_order : Bool
    , canceling_trade : Maybe String
    , viewPage : ViewPage
    , history : Maybe (List Order)
    , servers : Maybe (List ServerStock)
    , currentTime : Posix
    , zone : Zone
    , pickedTime : Maybe Posix
    , picker : DatePicker.DatePicker
    , serv_map : Dict.Dict Int String
    , history_sum : Float
    }



{-
   type Model = OldModel  |
       { netdata : NetDataSt
         , error : String
         , token : String
         -- input fields
         , username : String
         , password : String
         , order_note : String
         , order_bulk : String
         , order_server : String

         , text : LocalText
         --
         , selected_order : String
         , place_order_error : String
         , placing_order : Bool
         , canceling_trade : Maybe String


         , viewPage : ViewPage
         , history : Maybe (List Order)
         , servers : Maybe (List ServerStock)

         , currentTime : Posix
         , zone : Zone
         , pickedTime : Maybe Posix
         , picker : DatePicker.DatePicker

         , serv_map : Dict.Dict Int String

         , history_sum : Float
         }
         | UsersManageModel UsersManage.Model
-}


type NetworkData a
    = Data a
    | Querying
    | None


initial_model : Model
initial_model =
    { netdata = Loading
    , error = ""
    , token = "7B2270617373776F7264223A227869616F20313233222C2275736572223A227869616F687569227D"
    , username = ""
    , password = ""
    , order_note = ""
    , order_bulk = ""
    , order_server = ""
    , text = eng_text

    --
    , selected_order = ""
    , place_order_error = ""
    , placing_order = False
    , canceling_trade = Nothing
    , viewPage = ViewOrders
    , history = Nothing
    , servers = Nothing
    , currentTime = Time.millisToPosix 0
    , zone = Time.utc
    , pickedTime = Nothing
    , picker = DatePicker.init
    , serv_map = Dict.empty
    , history_sum = 0.0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initial =
            initial_model

        ----      task = Task.map2 Time.here Time.now
    in
    ( initial
    , Cmd.batch
        [ Task.perform SetCurrentTime Time.now
        , Task.perform AdjustTimeZone Time.here
        , query_status initial.token
        , server_cmd initial
        ]
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 5000 Tick


posixToDateString : Zone -> Posix -> String
posixToDateString zone date =
    ""



--    (Time.toDay zone date)
--        ++ "."
--        ++ (Time.toMonth zone date)
--        ++ "."
--        ++ String.fromInt (Time.toYear zone date)


posixToTimeString : Zone -> Posix -> String
posixToTimeString zone datetime =
    ""



--    (Time.toHour zone datetime)
--        ++ ":"
--        ++ (Time.toMinute zone datetime)
--        ++ ":"
--        ++ (Time.toSecond zone datetime)


userDefinedDatePickerSettings : Zone -> Posix -> DatePicker.Settings Msg
userDefinedDatePickerSettings zone today =
    let
        defaults =
            DatePicker.defaultSettings zone UpdatePicker
    in
    { defaults
        | isDayDisabled = \clientZone datetime -> False
        , focusedDate = Just today
        , dateStringFn = posixToDateString

        --        , timePickerVisibility = NeverVisible
    }


update msg model =
    case msg of
        Change f data ->
            ( f model data, Cmd.none )

        Tick _ ->
            ( model, query_status model.token )

        StatusRes sr ->
            ( update_status model sr, Cmd.none )

        LoginRes sr ->
            ( login_res model sr, Cmd.none )

        Login ->
            ( model, login_cmd model )

        SelectOrder order_uuid ->
            if model.selected_order == order_uuid then
                ( { model | selected_order = "" }, Cmd.none )

            else
                ( { model | selected_order = order_uuid }, Cmd.none )

        PlaceOrder ->
            ( { model | placing_order = True }, place_order_cmd model )

        PlaceOrderRes sr ->
            ( placeOrder_res model sr, Cmd.none )

        ArchiveClick order_uuid ->
            ( model, archive_order_cmd model order_uuid )

        CancelClick order ->
            ( model, Cmd.none )

        ServerListRes sr ->
            ( serverListRes model sr, Cmd.none )

        ShowStock ->
            ( { model | viewPage = ViewServers }, server_cmd model )

        ShowOrders ->
            ( { model | viewPage = ViewOrders }, Cmd.none )

        QueryHistory ->
            ( { model | viewPage = ViewHistory }, history_cmd model )

        ShowHistory ->
            ( { model | viewPage = ViewHistory }, history_cmd model )

        ShowEditTrade tradeInfo ->
            ( show_edit_trade model tradeInfo, Cmd.none )

        HistoryRes sr ->
            ( update_history model sr, Cmd.none )

        CancelTrade uuid ->
            cancel_trade model uuid

        ResetTradeRes sr ->
            ( resetTrade_res model sr, Cmd.none )

        OpenPicker ->
            ( { model
                | picker =
                    DatePicker.openPicker
                        (userDefinedDatePickerSettings model.zone model.currentTime)
                        model.currentTime
                        model.pickedTime
                        model.picker
              }
            , Cmd.none
            )

        UpdatePicker ( newPicker, maybeNewTime ) ->
            let
                npicker =
                    case maybeNewTime of
                        Just x ->
                            DatePicker.init

                        Nothing ->
                            newPicker

                nmodel =
                    { model
                        | picker = npicker
                        , pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime
                    }
            in
            ( nmodel
            , if nmodel.pickedTime /= model.pickedTime then
                history_cmd nmodel

              else
                Cmd.none
            )

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

        ResetTrade ->
            case model.viewPage of
                ViewEditTrade tradeInfo ->
                    ( { model | viewPage = ViewEditTrade { tradeInfo | error = Nothing } }, reset_trade_cmd model tradeInfo )

                _ ->
                    ( model, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        SetCurrentTime newTime ->
            ( { model | currentTime = newTime }, Cmd.none )


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


serv_map : List ServerStock -> Dict.Dict Int String
serv_map list =
    Dict.fromList (List.map (\x -> ( x.serverId, x.name )) list)


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


resetTrade_res model sr =
    case model.viewPage of
        ViewEditTrade tradeInfo ->
            case sr of
                Ok fullText ->
                    case Decode.decodeString decode_placeOrder_res fullText of
                        Ok res ->
                            if res.result == "ok" then
                                { model | viewPage = ViewOrders }

                            else
                                { model | viewPage = ViewEditTrade { tradeInfo | error = Just res.details } }

                        Err desc ->
                            let
                                err =
                                    Decode.errorToString desc
                            in
                            { model | error = err }

                _ ->
                    model

        _ ->
            model


login_res s sr =
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_login fullText of
                Ok login_info ->
                    { s | token = login_info.token }

                Err desc ->
                    let
                        err =
                            Decode.errorToString desc
                    in
                    { s | error = err }

        _ ->
            s


update_status s sr =
    case sr of
        Ok fullText ->
            case Decode.decodeString decode_error fullText of
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


login_form model =
    Html.div []
        [ Html.div [] [ Html.text model.error ]
        , Html.div [] [ Html.text "gotta login" ]
        , Html.div [] [ Html.text "username:", Html.input [ onInput (Change (\s x -> { s | username = x })) ] [ Html.text model.username ] ]
        , Html.div [] [ Html.text "pass:", Html.input [ onInput (Change (\s x -> { s | password = x })) ] [ Html.text model.password ] ]
        , Html.div [] [ Html.button [ onClick Login ] [ Html.text "login" ] ]
        ]


view s =
    case s.netdata of
        Loading ->
            Html.div []
                [ Html.text s.error
                , Html.br [] []
                , Html.text "loading"
                ]

        NotLoggedIn ->
            login_form s

        ND netdata ->
            case s.viewPage of
                ViewOrders ->
                    Html.div []
                        [ view_error s.text s.error
                        , view_details s.text netdata.user
                        , view_input_order s
                        , view_orders s netdata.orders s.selected_order
                        ]

                ViewServers ->
                    Html.div []
                        [ view_error s.text s.error
                        , view_details s.text netdata.user
                        , view_servers s
                        ]

                ViewHistory ->
                    Html.div []
                        [ view_error s.text s.error
                        , view_details s.text netdata.user
                        , view_history s
                        ]

                ViewEditTrade tradeInfo ->
                    let
                        error =
                            case tradeInfo.error of
                                Just something ->
                                    [ Html.text something
                                    ]

                                Nothing ->
                                    []
                    in
                    Html.div []
                        [ Html.div
                            [ Html.Attributes.style "color" "red"
                            , Html.Attributes.style "colspan" "1"
                            ]
                            error

                        -- onInput ChangeEditTradeText
                        , Html.div
                            [ Html.Attributes.style "colspan" "1"
                            ]
                            [ Html.textarea
                                [ onInput TradeEditChangeText
                                ]
                                [ Html.text tradeInfo.text ]
                            ]
                        , Html.div []
                            [ Html.button [ onClick ShowOrders ] [ Html.text "cancel" ]
                            , Html.button [ onClick ResetTrade ] [ Html.text "accept" ]
                            ]
                        ]
