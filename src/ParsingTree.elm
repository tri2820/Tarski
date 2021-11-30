module ParsingTree exposing (..)
import Dict exposing (Dict)
import Dict
import Maybe.Extra

type ParsingTree = Var String | Atom String | Fork ParsingTree ParsingTree
type MatchResult = Match (Dict String ParsingTree) | InvalidMatch ParsingTree ParsingTree | VariableCollision String ParsingTree ParsingTree
-- type RespondResult = Result ParsingTree | Err String

print : ParsingTree -> String
print tree = case tree of 
  Atom s -> s
  Var v -> v
  Fork l r -> let content = (print l) ++ " " ++ (print r)
    in case l of 
      Var _ -> content
      _ -> "(" ++ content ++ ")"

notSame : a -> a -> Maybe (a, a)
notSame v other_v = if other_v == v then Nothing else Just (other_v, v)
findInDict : Dict comparable c -> comparable -> c -> Maybe (c, c)
findInDict d k v = Dict.get k d |> Maybe.andThen (notSame v)

match : ParsingTree -> ParsingTree -> MatchResult
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
              dictMaybeCollisions =  Dict.map (findInDict union) r_dict 
              maybeCollisions = Dict.toList dictMaybeCollisions
              collisions = List.filter (Tuple.second >> Maybe.Extra.isJust) maybeCollisions
              result = case collisions of
                (var, Just (t1, t2))::_ -> VariableCollision var t1 t2
                _ -> Match (Dict.union l_dict r_dict)
            in result
          (Match _, err) -> err
          (err, _) -> err
    _ -> InvalidMatch p a

project : Dict String ParsingTree -> ParsingTree -> ParsingTree
project d t = case t of
    (Atom _) -> t
    (Fork a b) -> Fork (project d a) (project d b)
    (Var v) -> case Dict.get v d of 
      Nothing -> Var v
      Just varTree -> varTree

respond : ParsingTree -> ParsingTree -> ParsingTree -> Result String ParsingTree
respond pattern input replaced = case match pattern input of
  Match d -> Ok (project d replaced)
  InvalidMatch t1 t2 -> Err ("Cannot match " ++ (print t1) ++ " with " ++ (print t2))
  VariableCollision v t1 t2 -> Err ("Variable " ++ v ++ " is set to both " ++ (print t1) ++ " and " ++ (print t2))
