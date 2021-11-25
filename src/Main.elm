module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Maybe.Extra

-- MAIN


main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Tree

type Tree = Var String | Atom String | Fork Tree Tree

testTree : Tree
testTree = Fork 
  (Atom "D")
  (Fork
    (Fork
      (Atom "B")
      (Fork
        (Var "x")
        (Var "y")
      )
    )
    (Fork
      (Atom "A")
      (Fork
        (Var "x")
        (Var "y")
      )
    )
  )

testTree2 : Tree
testTree2 = Fork
  (Atom "K")
  (Fork
    (Atom "G")
    (Var "x")
  )

testTree3 : Tree
testTree3 = Fork
  (Atom "K")
  (Fork
    (Fork
      (Atom "G")
      (Var "x")
    )
    (Fork
      (Fork
        (Atom "M")
        (Var "y")
      )
      (Var "z")
    )
  )
  
testTree4 = Fork 
  (Atom "K")
  (Fork
    (Atom "G")
    (Fork
      (Atom "M")
      (Var "y")
    )
  )

testTree5 = Fork 
  (Atom "K")
  (Fork
    (Fork
      (Atom "M")
      (Var "y")
    )
    (Atom "G")
  )

testTree6 = Fork 
  (Atom "K")
  (Var "x")

testTree7 = Fork 
  (Atom "K")
  (Fork
    (Var "x")
    (Var "y")
  )

testTree8 = Fork 
  (Atom "K")
  (Fork
    (Var "x")
    (Var "x")
  )


init : Model
init = testTree5

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


view : Model -> Html Msg
view model =
  div []
    [ 
    -- div [] [ text (print model) ]
    div [] [ text (Debug.toString (match testTree7 testTree8)) ]
    ]