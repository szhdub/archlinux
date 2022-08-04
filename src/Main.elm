module Main exposing (..)

import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Lang exposing (..)
import LegacyPage exposing (..)
import OrdersManage exposing (..)
import Time
import UsersManage exposing (..)
import Json.Decode as Decode exposing (Value, decodeString, decodeValue, Decoder)

type Model1
    = UsersManageModel UsersManage.Model
    | LegacyPageModel LegacyPage.Model
    | OrdersManageModel OrdersManage.Model


type alias Model =
    { m1 : Model1, session : String }


type Msg
    = UsersManageMsg UsersManage.Msg
    | LegacyPageMsg LegacyPage.Msg
    | OrdersManageMsg OrdersManage.Msg
    | Tick Time.Posix
    | SetView NewView


type NewView
    = UsersManageView
    | OrdersManageView
    | HistoryManageView
    | StockManageView



main : Program ((String)) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions _ =
    Time.every 3000 Tick


init : String  -> ( Model, Cmd Msg )
init flags =
    let
        posts =
            case Decode.decodeString decodeStored flags of
                Ok postsJson ->
                    postsJson.token

                Err _ ->
                    "无效的token"

        ( model, msg ) =
            OrdersManage.init flags
    in
    ( { m1 = OrdersManageModel model, session = posts }, Cmd.map OrdersManageMsg msg )

decodeStored : Decoder LoginInfo
decodeStored =
    Decode.map LoginInfo
        (Decode.field "token" Decode.string)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.m1, msg ) of
        ( UsersManageModel usersmanageModel, Tick x ) ->
            let
                ( acc, ntask ) =
                    UsersManage.update (UsersManage.Tick x) usersmanageModel
            in
            ( { model | m1 = UsersManageModel acc }, Cmd.map UsersManageMsg ntask )

        ( UsersManageModel usersmanageModel, UsersManageMsg x ) ->
            let
                ( acc, ntask ) =
                    UsersManage.update x usersmanageModel
            in
            ( { model | m1 = UsersManageModel acc }, Cmd.map UsersManageMsg ntask )

        ( OrdersManageModel ordersmanageModel, Tick x ) ->
            let
                ( acc, ntask ) =
                    OrdersManage.update (OrdersManage.Tick x) ordersmanageModel
            in
            ( { model | m1 = OrdersManageModel acc }, Cmd.map OrdersManageMsg ntask )

        ( OrdersManageModel ordersmanageModel, OrdersManageMsg x ) ->
            let
                ( acc, ntask ) =
                    OrdersManage.update x ordersmanageModel
            in
            ( { model | m1 = OrdersManageModel acc }, Cmd.map OrdersManageMsg ntask )

        ( _, SetView newView ) ->
            ( { model | m1 = setView model newView }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


setView : Model -> NewView -> Model1
setView model newView =
    case newView of
        UsersManageView ->
            UsersManageModel UsersManage.init

        OrdersManageView ->
            let
                ( mo, _ ) =
                    OrdersManage.init model.session
            in
            OrdersManageModel mo

        StockManageView ->
            UsersManageModel UsersManage.init

        HistoryManageView ->
            UsersManageModel UsersManage.init


view : Model -> Html Msg
view { m1 } =
    let
        abody =
            case m1 of
                UsersManageModel submodel ->
                    Html.map UsersManageMsg (UsersManage.view submodel)

                LegacyPageModel submodel ->
                    Html.map LegacyPageMsg (LegacyPage.view submodel)

                OrdersManageModel submodel ->
                    Html.map OrdersManageMsg (OrdersManage.view submodel)
    in
    div []
        [ div []
            [ mainmenu
            ]
        , abody
        ]


mainmenu : Html Msg
mainmenu =
    div []
        [ Html.button [ onClick (SetView OrdersManageView) ] [ text Lang.eng_text.orders ]
        , Html.button [ onClick (SetView StockManageView) ] [ text Lang.eng_text.stock ]
        , Html.button [ onClick (SetView HistoryManageView) ] [ text Lang.eng_text.history ]
        , Html.button [ onClick (SetView UsersManageView) ] [ text Lang.eng_text.usersmanage ]
        ]
