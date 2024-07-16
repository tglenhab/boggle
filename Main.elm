module Main exposing (..)

import Random exposing (Generator)
import Random.List
import Html exposing (Html, div, span, text, button, h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Browser
    
type alias Model
    = Maybe Board

type Msg
    = GetNew
    | Generated Board

type alias Board
    = List String

type alias Die
    = List String

dice : List Die
dice =
    [ ["R", "I", "F", "O", "B", "X"]
    , ["I", "F", "E", "H", "E", "Y"]
    , ["D", "E", "N", "O", "W", "S"]
    , ["U", "T", "O", "K", "N", "D"]
    , ["H", "M", "S", "R", "A", "O"]
    , ["L", "U", "P", "E", "T", "S"]
    , ["A", "C", "I", "T", "A", "O"]
    , ["Y", "L", "G", "K", "U", "E"]
    , ["B", "M", "J", "O", "A", "Qu"]
    , ["E", "H", "I", "S", "P", "N"]
    , ["V", "E", "T", "I", "G", "N"]
    , ["B", "A", "L", "I", "Y", "T"]
    , ["E", "Z", "A", "V", "N", "D"]
    , ["R", "A", "L", "E", "S", "C"]
    , ["U", "W", "I", "L", "R", "G"]
    , ["P", "A", "C", "E", "M", "D"]
    ]
      
newBoard : Cmd Msg
newBoard =
    let
        getElem : List String -> Generator String
        getElem l =
            case l of
                head :: tail -> Random.uniform head tail
                [] -> Random.constant "ERROR"

        combineGens : List (Generator a) -> Generator (List a)
        combineGens l =
            case l of
                [] -> Random.constant []
                head :: tail -> Random.map2 (::) head (combineGens tail) 
    in
        Random.generate
            Generated 
            ((combineGens (List.map getElem dice))
            |> Random.andThen Random.List.shuffle)

            
main : Program () Model Msg
main =
    Browser.document
        { init =
            (\ flags ->
                ( Nothing
                , newBoard
                )
            )
        , update = update
        , view = view
        , subscriptions = subs
        }

    
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetNew -> (Nothing, newBoard)
        Generated b -> (Just b, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Boggle"
        [ div [ style "display" "flex"
              , style "flex-direction" "column"
              , style "align-items" "center"
              ]
              [ renderBoard model
              , button [onClick GetNew, style "margin" "20px"] [text "New Board"]
              ]
        ]

renderBoard : Model -> Html Msg
renderBoard model =
    case model of
        Nothing -> span [] []
        Just b ->
            div [ style "display" "flex"
                , style "flex-direction" "column"
                , style "flex" "1 1 0px"
                ]
                [ renderLine 0  4 b
                , renderLine 4  4 b
                , renderLine 8  4 b
                , renderLine 12 4 b
                ]

renderLine : Int -> Int -> Board -> Html Msg
renderLine start len board =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "flex" "1 1 0px"
        , style "flex-basis" "100%"
        ]
        (List.map (\ s -> span [ style "border" "2px solid black"
                               , style "width" "150px"
                               , style "height" "150px"
                               , style "display" "flex"
                               , style "align-items" "center"
                               , style "justify-content" "center"
                               ]
                       [h1 [] [text s]])
             (List.take len (List.drop start board))
        )


subs _ =
    Sub.none
