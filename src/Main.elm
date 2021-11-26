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
    (Var "x")
  )

  (Fork 
    (Fork 
      (Atom "B")
      (Var "x")
    )
    
    (Fork 
      (Atom "C")
      (Fork 
        (Atom "V")
        (Var "y")
      )
    )
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

bootstrapStylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "href"      "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]
        children = []
    in 
        node tag attrs children

customStylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "href"      "custom.css"
            ]
        children = []
    in 
        node tag attrs children


display : Tree -> Html Msg
display tree = case tree of 
  Atom s -> text s
  Var v -> text v
  Fork l r -> let content = span [] [display l, text " ", display r]
    in case l of 
      Atom _ -> span [ class "markHover" ] [text "(", content, text ")"]
      _ -> content


view : Model -> Html Msg
view model =
    div []
      [ 
      div [] [ customStylesheet, display model ]
      -- div [] [ text (Debug.toString (match testTree7 testTree8)) ]
      ]