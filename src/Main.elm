module Main exposing (..)

import Browser
import Html exposing (Html, div, text, span, node, p)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, onMouseOver)
import Html.Attributes exposing (class)
import Dict exposing (Dict)
import Maybe.Extra
import Html.Attributes exposing (attribute, style)
import Html.Attributes.Extra exposing (empty)
-- import Random

-- MAIN


main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type Mode = ModeNoSelected | ModeSelected Line
type alias Model = {
    mode: Mode, 
    trees: List Tree
  }

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

clickTree4 : Tree 
clickTree4 = Fork
  (Atom "A")

  (Fork
    (Var "x")

    (Fork
      (Atom "C")
      (Var "y")
    )
  )



init : Model
init = {
    mode = ModeNoSelected, 
    trees = [ testTree, treePattern, treeToBeReplaced, clickTree, clickTree2, clickTree3, clickTree4 ]
  }

print : Tree -> String
print tree = case tree of 
  Atom s -> s
  Var v -> v
  Fork l r -> let content = (print l) ++ " " ++ (print r)
    in case l of 
      Var _ -> content
      _ -> "(" ++ content ++ ")"

type MatchResult = Match (Dict String Tree) | InvalidMatch Tree Tree | VariableCollision String Tree Tree
notSame : a -> a -> Maybe (a, a)
notSame v other_v = if other_v == v then Nothing else Just (other_v, v)
findInDict : Dict comparable c -> comparable -> c -> Maybe (c, c)
findInDict d k v = Dict.get k d |> Maybe.andThen (notSame v)

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

-- UPDATE
type alias Line = Tree

type Msg = TreeClicked Tree | LineClicked Line | HighlightBlock Tree

update : Msg -> Model -> Model
update msg model = model

-- VIEW
-- type Reducibility = Atomic | Head | Tail Tree
-- type DisplayBracket = WithBracket | WithoutBracket
-- type DisplayTree = Node String | Branch DisplayBracket (Reducibility, DisplayTree) (Reducibility, DisplayTree)
-- type alias DisplayStructure = (Reducibility, DisplayTree)

-- -- There could be a better container type for Bracket but whatever
-- unBracket : DisplayTree -> DisplayTree
-- unBracket dtree = case dtree of
--   Node s -> Node s
--   Branch _ l r -> Branch WithoutBracket l r

-- display : Reducibility -> Tree -> DisplayStructure
-- display red tree = 
--   case tree of 
--       Atom s -> (red, Node s)
--       Var v -> (red, Node v)
--       Fork l r -> 
--         let           
--           morphismRed = case l of
--             Atom _ -> Atomic
--             _ -> red

--           (lRed, rRed) = if morphismRed == Atomic then (Atomic, Atomic) else (Head, Tail r)
--           bracket = case l of
--              Var _ -> WithoutBracket
--              _ -> WithBracket

--           left = display lRed l
--           right = display rRed r
--         in (red, Branch bracket left right)

-- displayTree : DisplayStructure -> Html Msg
-- displayTree (red, dt) = 
--   let
--     (eventContainer, hoverDetector) = case red of
--       Tail t -> (span [ class "markHover", class "highlightReducible", onClick (TreeClicked t) ], text >> List.singleton >> span [onMouseEnter (HighlightBlock t)] )
--       _ -> ( span [], text >> List.singleton >> span [] )

--     contentHtml = case dt of
--         Node s -> [ text s ]
--         Branch bracket left right -> let content = [displayTree left, hoverDetector " ", displayTree right] in case bracket of
--           WithBracket -> (hoverDetector "(") :: content ++ [hoverDetector ")"]
--           WithoutBracket -> content 
--   in contentHtml |> eventContainer
  
            
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

-- toDiv : Mode -> (Line, Maybe DisplayStructure) -> Html Msg
-- toDiv mode (line, cachedDisplayStructure) = 
--   let
--     lineClickHandler = case mode of
--       ModeSelected _ -> empty
--       ModeNoSelected -> onClick (LineClicked line)
--     displayStructure = case cachedDisplayStructure of
--       Nothing ->  display Head line
--       Just ds -> ds
--   in displayStructure |> Tuple.mapSecond unBracket >> displayTree >> List.singleton >> div [ lineClickHandler , class "line" ]

view : Model -> Html Msg
view model = 
  let
    -- treeDivs = List.map (toDiv model.mode) model.lines
    t =  testTree |> toDTree >> toHTML 
    _ = Debug.log "p" (print testTree)
    _ = Debug.log "tree" t
    treeDiv  = t |> List.singleton
    barText = case model.mode of
      ModeNoSelected -> "SELECT a line as material"
      ModeSelected line -> "Selected > " ++ (print line)
  in
    div [ ] [ 
        node "link" [ attribute "rel" "stylesheet", attribute "href" "custom.css" ] [],
        node "link" [ attribute "rel" "stylesheet", attribute "href" "//cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css" ] [],
        div [] treeDiv,
        div [ class "bar" ] [ text barText ]
      ]


type Treelike o = One o | Two o (Treelike o) (Treelike o)
zipTree : Treelike a -> Treelike b -> Treelike (a,b)
zipTree ta tb = case (ta, tb) of
  (One a, One b) -> One (a,b)
  (One a, Two b _ _) -> One (a,b)
  (Two a _ _, One b) -> One (a,b)
  (Two a la ra, Two b lb rb) -> Two (a,b) (zipTree la lb) (zipTree ra rb)

type F a = F a (Tree -> F a) (Tree -> F a)
show : Tree -> (Tree -> F a)  -> Treelike a
show tree f = 
  let
    (F m g h) = f tree
  in case tree of
    Atom s -> One m
    Var v -> One m
    Fork l r -> Two m (show l g) (show r h)

type Bracket = YesBracket | NoBracket
mkBracket : Tree -> F Bracket
mkBracket tree = case tree of
  Fork (Atom _) _ -> F YesBracket mkBracket mkBracket
  Fork (Fork _ _) _ -> F YesBracket mkBracket mkBracket
  _ -> F NoBracket mkBracket mkBracket

mkTree : Tree -> F (Maybe String)
mkTree tree = case tree of
  Fork _ _ -> F Nothing mkTree mkTree
  Atom a -> F (Just a) mkTree mkTree
  Var v -> F (Just v) mkTree mkTree

type Re = A | H | T
dec : Re -> Tree -> F Re
dec x t = case (x,t) of 
  (A, _) -> F x (dec A) (dec A)
  (_, Fork (Atom _) _) -> F x (dec A) (dec A)
  (_, Atom _) -> F x (dec x) (dec x)
  (_, Var _) -> F x (dec x) (dec x)
  _ -> F x (dec H) (dec T)
mkRe : Tree -> F Re
mkRe = dec T

type alias DTree = Treelike (Bracket, (Re, Maybe String))
toDTree : Tree -> Treelike (Bracket, (Re, Maybe String))
toDTree tree = 
  let
    f = show tree
    bracketT = f mkBracket
    reT = f mkRe
    cT = f mkTree
    t = zipTree bracketT <| zipTree reT cT

  in t

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
