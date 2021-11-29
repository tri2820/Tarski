module Main exposing (..)

import Browser
import Html exposing (Html, div, text, node, span)
import Html.Attributes exposing (class)
import Html.Attributes exposing (attribute)
import Html.Attributes.Extra exposing (empty)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (goToChild)
import Maybe exposing (andThen)
import Treelike exposing (Re)
import Treelike exposing (Bracket)
import ParsingTree exposing (ParsingTree)
import Treelike exposing (Bracket(..))
import Treelike exposing (Re(..))
import ParsingTree exposing (ParsingTree(..))
import Treelike exposing (mkBracket)
import Treelike exposing (F(..))
import Treelike exposing (mkRe)
import TestTrees exposing (testTree)
import TestTrees exposing (clickTree)
import TestTrees exposing (clickTree2)
import TestTrees exposing (clickTree3)
import TestTrees exposing (treeToBeReplaced)
import MultiwayTreeZipper exposing (Zipper)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Events exposing (stopPropagationOn)
import Html.Events exposing (onMouseEnter)
import Html.Events exposing (onMouseOver, on)
import Json.Decode as Json
import Maybe.Extra
import MultiwayTreeZipper exposing (goToRoot)
import MultiwayTreeZipper exposing (updateDatum)
import Html.Attributes exposing (style)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type Mode = ModeNoSelected

type alias RootTree = Tree DisplayRecord
type alias Model = {
    -- lines : List (ParsingTree, Maybe (Tree DisplayRecord))
    root : RootTree
    -- parsingTrees : List (ParsingTree)
  }

emptyDisplayRecord : DisplayRecord
emptyDisplayRecord = {
    reducibility = T,
    bracket = NoBracket,
    tree = Fork (Atom "") (Atom ""),
    highlight = False
  }

init : Model
init = {
  -- parsingTrees = [],
  root =  [testTree, clickTree, clickTree2, clickTree3, clickTree3, treeToBeReplaced] |> List.map rootConvert >> Tree emptyDisplayRecord
    -- lines = List.map (\s -> (s, Nothing)) [testTree, clickTree, clickTree2, clickTree3, clickTree3, treeToBeReplaced]
  }

type Msg = Reduce (Zipper DisplayRecord) | Highlight (Zipper DisplayRecord) | UnHighlight (Zipper DisplayRecord)
update : Msg -> Model -> Model
update msg model = case msg of
  Reduce z -> 
    let 
      _ = Debug.log "z" z
    in 
      model
  Highlight z ->
    let 
      _ = Debug.log "hover" z
      root = updateDatum (\record -> {record | highlight = True}) z
        |> andThen goToRoot 
        |> Maybe.map Tuple.first
    in case root of
      Just t -> { model | root = t }
      Nothing -> model

  UnHighlight z ->
    let 
      _ = Debug.log "hover" z
      root = updateDatum (\record -> {record | highlight = False}) z
        |> andThen goToRoot 
        |> Maybe.map Tuple.first
    in case root of
      Just t -> { model | root = t }
      Nothing -> model

view : Model -> Html Msg
view model = 
  let 
    -- _ = Debug.log "trees" model.trees
    z : Zipper DisplayRecord
    z = model.root |> \s -> (s, [])
    c = zipToRoot z
  in
    div [ ] [ 
        node "link" [ attribute "rel" "stylesheet", attribute "href" "custom.css" ] [],
        node "link" [ attribute "rel" "stylesheet", attribute "href" "//cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css" ] [],
        c
      ]

type alias DisplayRecord = {
    reducibility : Re,
    bracket: Bracket,
    tree: ParsingTree,
    highlight: Bool
  }

walker : (ParsingTree -> F Re) -> (ParsingTree -> F Bracket) -> ParsingTree -> F (Re, Bracket)
walker mkR mkBra tree = 
  let
    (F redu leftReduF rightReduF) = mkR tree
    (F bracket leftBracketF rightBracketF) = mkBra tree
  in F (redu, bracket) (walker leftReduF leftBracketF) (walker rightReduF rightBracketF)
rootWalker : ParsingTree -> F (Re, Bracket)
rootWalker = walker mkRe mkBracket

convert : (ParsingTree -> F (Re, Bracket)) -> ParsingTree -> Tree DisplayRecord
convert w tree = 
  let
    (F (re,bra) wLeft wRight) = w tree
    record = { reducibility = re, bracket = bra, tree = tree, highlight = False }
    children = case tree of 
      Fork l r -> [ convert wLeft l, convert wRight r] 
      _ -> []
  in Tree record children

rootConvert : ParsingTree -> Tree DisplayRecord
rootConvert = convert rootWalker

brhtml : Bracket -> Html Msg -> List (Html Msg)
brhtml br = case br of 
  YesBracket ->  \html -> [ text "(", html , text ")" ]
  NoBracket -> \html -> [ html ]

onMouseOverStopPropagation : a -> Html.Attribute a
onMouseOverStopPropagation msg = stopPropagationOn "mouseover" <| Json.succeed ( msg, True )
onMouseOutStopPropagation : a -> Html.Attribute a
onMouseOutStopPropagation msg = stopPropagationOn "mouseout" <| Json.succeed ( msg, True )
eventNode : Re -> Zipper DisplayRecord -> List (Html Msg) -> Html Msg
eventNode re z = case re of 
  T -> span [ class "markHover", class "highlightReducible", onClickStopPropagation (Reduce z), onMouseOverStopPropagation (Highlight z) , onMouseOutStopPropagation (UnHighlight z)]
  _ -> span []

zipperToHTML : Zipper DisplayRecord -> Html Msg
zipperToHTML z = case z of
  (Tree { reducibility, bracket, tree, highlight } _, _) -> 
    let
      content = case tree of 
        Var v -> [ text v ]
        Atom a -> [ text a ]
        _ -> 
          let
            zipperLeft = goToChild 0 z
            zipperRight = goToChild 1 z
          in case (zipperLeft, zipperRight) of 
            (Just zl, Just zr) -> [zipperToHTML zl, text " ", zipperToHTML zr]
            _ -> []
    in content |> span [] >> brhtml bracket >> eventNode reducibility z >> highlightNode highlight

highlightNode : Bool -> Html msg -> Html msg
highlightNode highlight = if highlight then \x -> span [ style "background-color" "red" ] [x] else \x -> span [][x]

type alias ZipperRoot = Zipper DisplayRecord
zipToRoot : ZipperRoot -> Html Msg
zipToRoot rootZipper = case rootZipper of
  (Tree _ children, _) -> 
    let
      htmls = List.length children 
        |> List.range 0 
        |> List.map (
            (\i -> goToChild i rootZipper) 
            >> Maybe.andThen (updateDatum (\record -> {record | bracket = NoBracket, reducibility = H})) 
            >> Maybe.map zipperToHTML
            )
        |> Maybe.Extra.values
        |> List.map (\html -> div [] [html])
    in span [ style "font-size" "40px" ] htmls

