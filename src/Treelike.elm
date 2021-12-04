module Treelike exposing (..)
import ParsingTree exposing (ParsingTree(..))

type Treelike o = One o | Two o (Treelike o) (Treelike o)
type F a = F a (ParsingTree -> F a) (ParsingTree -> F a)
type Bracket = YesBracket | NoBracket
type Re = A | H | T

type alias DisplayTree = Treelike {
    bracket : Bracket,
    reducibility : Re,
    value: Maybe String
  }

zipTree : Treelike a -> Treelike b -> Treelike (a, b)
zipTree ta tb = case (ta, tb) of
  (One a, One b) -> One (a,b)
  (One a, Two b _ _) -> One (a,b)
  (Two a _ _, One b) -> One (a,b)
  (Two a la ra, Two b lb rb) -> Two (a,b) (zipTree la lb) (zipTree ra rb)

label : ParsingTree -> (ParsingTree -> F a)  -> Treelike a
label tree f = 
  let
    (F m g h) = f tree
  in case tree of
    Atom _ -> One m
    Var _ -> One m
    Fork l r -> Two m (label l g) (label r h)

-- read the last bracket, if this time it's a fork again font make bracket
-- mkBracket : ParsingTree -> F Bracket
-- mkBracket tree = case tree of
--   Fork (Atom _) _ -> F NoBracket mkBracket mkBracket
--   Fork (Fork _ _) _ -> F YesBracket mkBracket mkBracket
--   -- Fork (Fork _ _) _ -> F NoBracket mkBracket mkBracket
--   _ -> F NoBracket mkBracket mkBracket

brack : Bracket -> ParsingTree -> F Bracket
brack bracket tree = case tree of
  Fork (Fork _ _) _ -> F bracket (brack YesBracket) (brack YesBracket)
  Var _ -> F NoBracket (brack NoBracket) (brack NoBracket)
  _ -> F bracket (brack NoBracket) (brack NoBracket)

mkBracket : ParsingTree -> F Bracket
mkBracket = brack NoBracket

mkTree : ParsingTree -> F (Maybe String)
mkTree tree = case tree of
  Fork _ _ -> F Nothing mkTree mkTree
  Atom a -> F (Just a) mkTree mkTree
  Var v -> F (Just v) mkTree mkTree

dec : Re -> ParsingTree -> F Re
dec x t = case (x,t) of 
  (A, _) -> F x (dec A) (dec A)
  (_, Fork (Atom _) _) -> F x (dec A) (dec A)
  (_, Atom _) -> F x (dec x) (dec x)
  (_, Var _) -> F x (dec x) (dec x)
  _ -> F x (dec H) (dec T)
mkRe : ParsingTree -> F Re
mkRe = dec T

toDTree : ParsingTree -> Treelike (Bracket, (Re, Maybe String))
toDTree tree = 
  let
    f = label tree
    bracketT = f mkBracket
    reT = f mkRe
    cT = f mkTree
    t = zipTree bracketT <| zipTree reT cT
  in t

map : (o -> a) -> Treelike o -> Treelike a
map f t = case t of 
  One x -> One (f x)
  Two x l r -> Two (f x) (map f l) (map f r)

toRecord : (a, (b, c)) -> { bracket : a, reducibility : b, value : c }
toRecord (br, (re, str)) = {
    bracket = br,
    reducibility = re, 
    value = str
  }

toDisplayTree : ParsingTree -> Treelike { bracket : Bracket, reducibility : Re, value : Maybe String}
toDisplayTree = toDTree >> (map toRecord)
