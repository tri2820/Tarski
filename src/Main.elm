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
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Json
import Maybe.Extra
import MultiwayTreeZipper exposing (goToRoot)
import MultiwayTreeZipper exposing (updateDatum)
import Html.Attributes exposing (style)
import ParsingTree exposing (print)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }



type Mode = ModeIdle | ModeHolding LinePointer

type alias RootTree = Tree DisplayRecord
type alias RootPointer = Zipper DisplayRecord
type alias BlockPointer = Zipper DisplayRecord
type alias LinePointer = Zipper DisplayRecord
type alias Model = {
    mode : Mode,
    root : RootTree
    -- parsingTrees : List (ParsingTree)
  }

emptyDisplayRecord : DisplayRecord
emptyDisplayRecord = {
    reducibility = T,
    bracket = NoBracket,
    tree = Fork (Atom "") (Atom ""),
    isHighlight = False
  }

init : Model
init = {
  mode = ModeIdle,
  root =  [testTree, clickTree, clickTree2, clickTree3, clickTree3, treeToBeReplaced] |> List.map rootConvert >> Tree emptyDisplayRecord
    -- lines = List.map (\s -> (s, Nothing)) [testTree, clickTree, clickTree2, clickTree3, clickTree3, treeToBeReplaced]
  }

type Msg = Reduce BlockPointer 
  | HighlightBlock BlockPointer 
  | UnHighlightBlock BlockPointer 
  | Choose LinePointer 
  | HighlightLine LinePointer 
  | UnHighlightLine LinePointer 
update : Msg -> Model -> Model
update msg model = case msg of
  Reduce z -> case model.mode of
    ModeHolding line -> highlight False { model | mode = ModeIdle } z
    _ -> model
  HighlightBlock z -> case model.mode of 
    ModeHolding _ -> highlight True model z
    ModeIdle -> model
  UnHighlightBlock z -> highlight False model z
  Choose z -> case model.mode of 
    ModeIdle -> highlight False { model | mode = ModeHolding z } z
    _ -> model
  HighlightLine z -> 
    let 
      _ = Debug.log "line hover" z
    in
      case model.mode of
        ModeIdle -> highlight True model z
        _ -> model
  UnHighlightLine z -> highlight False model z
     
highlight : Bool -> Model -> Zipper DisplayRecord -> Model
highlight isHighlight model z = 
  let
    updateHighlight = (\record -> { record | isHighlight = isHighlight })
    root = updateDatum updateHighlight z
        |> andThen goToRoot 
        |> Maybe.map Tuple.first
  in 
    case root of
      Just t -> { model | root = t }
      Nothing -> model

view : Model -> Html Msg
view model = 
  let 
    -- _ = Debug.log "trees" model.trees
    z : RootPointer
    z = model.root |> \s -> (s, [])
    c = zipToRoot model.mode z
    -- 
    actionBarText = case model.mode of
      ModeIdle -> "Choose a line by clicking"
      ModeHolding (Tree {tree} _, _) -> "Match with " ++ print tree
    actionBar = div [ class "bar" ] [text actionBarText]

  in
    div [ ] [ 
        node "link" [ attribute "rel" "stylesheet", attribute "href" "custom.css" ] [],
        node "link" [ attribute "rel" "stylesheet", attribute "href" "//cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css" ] [],
        c,
        actionBar
      ]

type alias DisplayRecord = {
    reducibility : Re,
    bracket: Bracket,
    tree: ParsingTree,
    isHighlight: Bool
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
    record = { reducibility = re, bracket = bra, tree = tree, isHighlight = False }
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
eventNode : Mode -> Re -> RootPointer -> List (Html Msg) -> Html Msg
eventNode mode re z = case (re, mode) of 
  (T, ModeHolding _) -> span [
      style "background-color" "rgba(0, 0, 255, 0.1)",
      onClickStopPropagation (Reduce z),
      onMouseOverStopPropagation (HighlightBlock z) , 
      onMouseOutStopPropagation (UnHighlightBlock z)
    ]
  _ -> span []

zipperToHTML : Mode -> RootPointer -> Html Msg
zipperToHTML mode z = case z of
  (Tree { reducibility, bracket, tree, isHighlight } _, _) -> 
    let
      content = case tree of 
        Var v -> [ text v ]
        Atom a -> [ text a ]
        _ -> 
          let
            zipperLeft = goToChild 0 z
            zipperRight = goToChild 1 z
          in case (zipperLeft, zipperRight) of 
            (Just zl, Just zr) -> [ zipperToHTML mode zl, text " ", zipperToHTML mode zr]
            _ -> []
    in content |> span [] >> brhtml bracket >> eventNode mode reducibility z >> highlightNode isHighlight

highlightNode : Bool -> Html msg -> Html msg
highlightNode isHighlight = if isHighlight then \x -> span [ style "background-color" "rgba(255, 0, 0, 0.3)" ] [x] else \x -> span [][x]



mkLine : Mode -> LinePointer -> List (Html.Attribute Msg)
mkLine mode z = case mode of
   ModeIdle -> [ 
      onClickStopPropagation (Choose z), 
      onMouseOverStopPropagation (HighlightLine z) , 
      onMouseOutStopPropagation (UnHighlightLine z) 
    ] 
   _ -> []

zipToRoot : Mode -> RootPointer -> Html Msg
zipToRoot mode rootZipper = case rootZipper of
  (Tree _ children, _) -> 
    let
      htmls = List.length children 
        |> List.range 0 
        |> List.map (
            (\i -> goToChild i rootZipper) 
            >> Maybe.andThen (updateDatum (\record -> {record | bracket = NoBracket, reducibility = H})) 
            >> Maybe.map (\z -> (z, zipperToHTML mode z))
            )
        |> Maybe.Extra.values
        |> List.map (\(z, html) -> div ((class "line")::(mkLine mode z)) [ html ])
    in span [ ] htmls

