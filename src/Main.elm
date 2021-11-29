module Main exposing (..)

import Browser
import Html exposing (Html, div, text, node, span)
import Html.Attributes exposing (class)
import Html.Attributes exposing (attribute)
import Html.Attributes.Extra exposing (empty)
import Tree exposing (Tree(..))
import TestTrees exposing (testTree, treePattern, treeToBeReplaced, clickTree, clickTree2, clickTree3, clickTree4)
import Treelike exposing (Bracket(..), Re(..), Treelike(..), toDTree)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type Mode = ModeNoSelected
type alias Model = {
    mode: Mode, 
    trees: List Tree
  }

init : Model
init = {
    mode = ModeNoSelected, 
    trees = [ testTree, treePattern, treeToBeReplaced, clickTree, clickTree2, clickTree3, clickTree4 ]
  }

type Msg = TreeClicked Tree
update : Msg -> Model -> Model
update msg model = model

view : Model -> Html Msg
view model = 
  let
    treeDiv  = testTree |> toDTree >> toHTML >> List.singleton
    barText = "SELECT a line as material"
  in
    div [ ] [ 
        node "link" [ attribute "rel" "stylesheet", attribute "href" "custom.css" ] [],
        node "link" [ attribute "rel" "stylesheet", attribute "href" "//cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css" ] [],
        div [] treeDiv,
        div [ class "bar" ] [ text barText ]
      ]

--
brhtml : Bracket -> Html Msg -> List (Html Msg)
brhtml br = case br of 
  YesBracket -> \html -> [ text "(", html, text ")" ]
  NoBracket -> \html -> [ html ]

container : Re -> List (Html msg) -> Html msg
container re = case re of 
  T -> span [ class "markHover", class "highlightReducible" ]
  _ -> span []

toHTML : Treelike (Bracket, (Re, Maybe String)) -> Html Msg
toHTML tree = case tree of
  One (bracket, (re, Just s)) -> text s |> brhtml bracket >> container re
  Two (bracket, (re, Nothing)) l r -> span [] [toHTML l, text " ", toHTML r] |> brhtml bracket >> container re
  _ -> span [][]
