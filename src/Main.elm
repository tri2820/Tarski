module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Dict exposing (Dict)
import Maybe.Extra
import Html.Attributes exposing (attribute)
import Html exposing (node)
import Html exposing (mark)
import Html exposing (p)
import Html exposing (pre)
import Html exposing (code)
import Html.Attributes.Extra exposing (empty)
import Tuple exposing (first)

-- MAIN


main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Mode = ModeNoSelected | ModeSelect String
type alias Model = (Mode, List Tree)

type Tree = Var String | Atom String | Fork Tree Tree

testTree : Tree
testTree = Fork 
  (Fork 
    (Atom "A")
    (Fork
      (Atom "D")
      (Atom "M")
    )
  )

  (Fork 
    (Fork 
      (Atom "D")
      (Atom "M")
    )
    
    (Fork 
      (Atom "C")
      (Fork 
        (Atom "V")
        (Fork 
          (Var "y")
          (Fork 
            (Var "z")
            (Var "w")
          )
        )
      )
    )
  )

treePattern : Tree
treePattern = Fork 
  (Fork 
    (Atom "A")
    (Var "x")
  )

  (Fork 
    (Var "x")
    
    (Fork 
      (Atom "C")
      (Var "k")
    )
  )

treeToBeReplaced : Tree
treeToBeReplaced = Fork
  (Fork 
    (Fork 
      (Atom "Head")
      (Var "y")
    )
    (Var "x")
  )

  (Fork 
    (Atom "Tail")
    (Var "k")
  )

clickTree : Tree 
clickTree = Fork
  (Fork 
    (Fork
      (Atom "A")
      (Var "z")
    )

    (Fork
      (Atom "B")
      (Var "x")
    )
  )

  (Fork 
    (Fork
      (Atom "A")
      (Var "z")
    )

    (Fork
      (Atom "B")
      (Var "x")
    )
  )

clickTree2 : Tree 
clickTree2 = Fork
  (Fork 
    (Fork
      (Atom "A")
      (Var "z")
    )

    (Fork
      (Atom "B")
      (Var "x")
    )
  )

  (Fork
    (Atom "C")
    (Var "k")
  )

clickTree3 : Tree 
clickTree3 = Fork
  (Atom "A")

  (Fork
    (Fork
      (Atom "B")
      (Var "x")
    )

    (Fork
      (Atom "C")
      (Var "y")
    )
  )


init : Model
init = (ModeNoSelected, [ testTree, treePattern, treeToBeReplaced, clickTree, clickTree2, clickTree3 ])

print : Tree -> String
print tree = case tree of 
  Atom s -> s
  Var v -> v
  Fork l r -> let content = (print l) ++ " " ++ (print r)
    in case l of 
      Atom _ -> "(" ++ content ++ ")"
      _ -> content

type MatchResult = Match (Dict String Tree) | InvalidMatch Tree Tree | VariableCollision String Tree Tree
match : Tree -> Tree -> MatchResult
match p a = case p of 
  Var v -> Match (Dict.singleton v a)
  Atom s -> case a of 
    Atom s_a -> if s == s_a then Match (Dict.empty) else InvalidMatch p a
    _ -> InvalidMatch p a
  Fork l r -> case a of 
    Fork l_a r_a -> case (match l l_a, match r r_a) of
          (Match l_dict, Match r_dict) ->
            let
              union = Dict.union l_dict r_dict
              dictMaybeCollisions = Dict.map (\k -> \v -> case Dict.get k union of
                  Just other_v -> if other_v == v then Nothing else Just (other_v, v)
                  Nothing -> Nothing
                 ) r_dict
              maybeCollisions = Dict.toList dictMaybeCollisions
              collisions = List.filter (\(_,v) -> Maybe.Extra.isJust v) maybeCollisions
              result = case collisions of
                (var, Just (t1, t2))::_ -> VariableCollision var t1 t2
                _ -> Match (Dict.union l_dict r_dict)
            in result
          (Match _, err) -> err
          (err, _) -> err
    _ -> InvalidMatch p a

-- UPDATE
type Msg = Unit


update : Msg -> Model -> Model
update msg model = 
  let 
    _ = Debug.log "Message" msg
  in 
    model



-- VIEW
type Reducibility = Atomic | Head | Tail
clickToReduce : Reducibility -> Html.Attribute msg
clickToReduce = \red -> case red of 
            Tail -> class "clickToReduce"
            _ -> empty

display : Reducibility -> Tree -> Html Msg
display red tree = 
  case tree of 
      Atom s -> span [clickToReduce red] [text s]
      Var v -> span [clickToReduce red] [text v]
      Fork l r -> 
        let           
          morphismRed = case l of
            Atom _ -> Atomic
            _ -> red

          (lRed, rRed) = case morphismRed of
            Atomic -> (Atomic, Atomic)
            _ -> (Head, Tail)

          left = display lRed l
          right = display rRed r
        in 
          case l of
            Var _ -> span [clickToReduce red] [left, text " ", right]
            _ -> span [clickToReduce red, class "markHover" ] [text "(", left, text " ", right, text ")"]

project : Dict String Tree -> Tree -> Tree
project d t = case t of
    (Atom _) -> t
    (Fork a b) -> Fork (project d a) (project d b)
    (Var v) -> case Dict.get v d of 
      Nothing -> Var v
      Just varTree -> varTree

type RespondResult = Result Tree | Err String
respond : Tree -> Tree -> Tree -> RespondResult
respond pattern input replaced = case match pattern input of
  Match d -> Result (project d replaced)
  InvalidMatch t1 t2 -> Err ("Cannot match" ++ (print t1) ++ "with" ++ (print t2))
  VariableCollision v t1 t2 -> Err ("Variable" ++ v ++ "is set to both" ++ (print t1) ++ "and" ++ (print t2))

view : Model -> Html Msg
view model =
  let 
    (mode, trees) = model
    treeDivs = List.map (display Head >> \s -> div [ class "line" ] [s])  trees
    barText = case mode of
      ModeNoSelected -> "SELECT a line as material"
      ModeSelect _ -> "MATCH a block to create a new theorem"
  in
    div [ ] [ 
        node "link" [ attribute "rel" "stylesheet", attribute "href" "custom.css" ] [],
        node "link" [ attribute "rel" "stylesheet", attribute "href" "//cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css" ] [],
        div [] treeDivs,
        div [ class "bar" ] [ text barText ]
      ]