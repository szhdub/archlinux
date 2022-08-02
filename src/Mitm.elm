module Mitm exposing (..)

import Html exposing (..)
import Html.Attributes
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)

import Html.Events exposing (onClick, onInput)

type alias Mapping = {
    uuid : String
    , ip : String
    , status : String
  }
type alias Model = {
    accounts : List Mapping 
    , uuid : String
    , ip : String
  }

type Place = IpEdit | UuidEdit

type Msg = AddMappingClick 
          | OnInput Place String
          | MappingsResponse 

init : Model
init = {
         accounts = [{ip="dasad", uuid="asss", status = ""}]
         , uuid = ""  
         , ip = ""
       }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnInput IpEdit x ->
      ({model | uuid = x}, Cmd.none) 
    OnInput UuidEdit x ->
      ({model | uuid = x}, Cmd.none) 
    AddMappingClick ->
      (model, Cmd.none)
    _ ->
      (model, Cmd.none)

view model =
  div [] [
      div [] [
        input [
           Html.Attributes.placeholder "192.168.2.1"
           , onInput (OnInput IpEdit)] []
        , input [
              Html.Attributes.placeholder "acc uuid"
              , onInput (OnInput UuidEdit)] []
        , button [ onClick AddMappingClick ] [text "add" ]
        ]
       ,  div [] [
        table [] ([
            tr [] [
                th [  ] [
                         text "ip"
                         ]
                , th [] [
                         text "uuid" 
                     ]
                , th [] [
                         text "status" 
                     ]
             ]
                ] ++ (view_mappings model))
      ]
    ]

view_mappings model = 
  List.map viewMapping model.accounts 

viewMapping mapping = 
        tr [] [
          td [] [
                   text mapping.ip
                   ]
          , td [] [
                   text mapping.uuid
                     ]
          , td [] [
                   text mapping.status
                     ]
             ]
      
decode_mapping = 
    Decode.succeed Mapping
     |> required "ip" Decode.string
     |> required "uuid" Decode.string
     |> required "status" Decode.string


