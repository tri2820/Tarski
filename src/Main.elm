module Main exposing (..)

import Browser
import Html exposing (Html, div, text, node, span)
import Html.Attributes exposing (class)
import Html.Attributes exposing (attribute)
import Html.Attributes.Extra exposing (empty)
import Tree exposing (Tree(..))
import TestTrees exposing (testTree, treePattern, treeToBeReplaced, clickTree, clickTree2, clickTree3, clickTree4)
import Treelike exposing (Bracket(..), Re(..), Treelike(..), toDTree)
import Treelike exposing (toDisplayTree)
import Treelike exposing (DisplayTree)

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
    treeDiv  = testTree |> toDisplayTree >> toHTML >> List.singleton
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

htmlText : Maybe String -> Html msg 
htmlText value = case value of 
  Just s -> text s
  Nothing -> text " "

mergeBranch : DisplayTree -> DisplayTree -> Html Msg -> Html Msg
mergeBranch l r = \middle -> span [] [toHTML l, middle, toHTML r]

toHTML : DisplayTree -> Html Msg
toHTML tree = case tree of
  One {bracket, reducibility, value} -> value |> htmlText >> brhtml bracket >> container reducibility
  Two {bracket, reducibility, value} l r -> value |> htmlText >> mergeBranch l r |> brhtml bracket >> container reducibility
