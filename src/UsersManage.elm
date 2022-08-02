module UsersManage exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Lang exposing (..)
import Time


type alias Model =
    { users : List User
    , took : Int
    , error : String
    }



--type Msg = UsersRs (Result Http.Error String)
--           | Tick Time.Posix


type Msg
    = Tick Time.Posix


type alias User =
    { username : String
    , credits : String
    , allow_credits : String
    }


initUserList : List User
initUserList =
    [ { username = "xiaohui", credits = "1000", allow_credits = "5000" }
    , { username = "xiaohui1", credits = "10000", allow_credits = "50000" }
    ]


init : Model
init =
    { users = initUserList
    , took = 10
    , error = "nothing"
    }


decode_user =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "credits" Decode.string
        |> required "allow_credits" Decode.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | users = initUserList }, Cmd.none )



--UsersRs sr -> (proc_users model False sr, Cmd.none)
-- _ ->
--    (model, Cmd.none)
{-
   proc_users model is_update sr =
       case sr of
          Ok fullText ->
            case (Decode.decodeString decode_user fullText) of
                  Ok response ->
                    { model |
                        users = response.users
                        , took = response.took
                        , error = ""
                    }
                  Err desc ->
                    let err = (Decode.errorToString desc)
                    in { model | error = err }

          _ ->
            { model | error = "network error" }

-}


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
                            [ text "选择用户名"
                            ]
                        ]
                    ]
                ]
            , div [ Html.Attributes.style "float" "left" ]
                [ table [ Html.Attributes.style "border" "1px solid" ]
                    [ tr []
                        [ td []
                            [ select []
                                (view_mappings model)
                            ]
                        ]
                    ]
                ]
            ]
        ]


view_mappings model =
    List.map viewUser model.users


viewUser : User -> Html Msg
viewUser user =
    option []
        [ text user.username
        ]



{-
   type alias UsersCountJson = {
          took : Int,
          users : List User
          }

-}
{-
   query_users_cmd model =
       let uri = "http://168.119.74.89:20030/get_users_jsx"
           body = Encode.encode 0 (Encode.object [
             ])

        in Http.post
              {
                url = uri
              , body = Http.stringBody "application/json" body
              , expect = Http.expectString UsersRs
              }



   type KV = List (String, T)
   type T = M (List KV) | TL (List T) | S String | I Int | B Bool
-}
