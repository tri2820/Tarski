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

-- MAIN


main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Tree

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

init : Model
init = testTree

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

type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model = model



-- VIEW

customStylesheet : Html msg
customStylesheet = node "link" [ attribute "rel" "stylesheet", attribute "href" "custom.css"] []

display : Tree -> Html Msg
display tree = case tree of 
  Atom s -> text s
  Var v -> text v
  Fork l r -> let content = span [] [display l, text " ", display r]
    in case l of 
      Atom _ -> span [ class "markHover" ] [text "(", content, text ")"]
      _ -> content

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
    res = respond treePattern testTree treeToBeReplaced
    _ = Debug.log "original tree" treeToBeReplaced
    _ = Debug.log "after tree" res
  in
    div []
      [ 
      div [] [ customStylesheet, display model ]
      ]