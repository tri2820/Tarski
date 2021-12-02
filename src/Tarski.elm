module Tarski exposing (..)
import ParsingTree exposing (ParsingTree(..))

fourVar a b c d = (Fork (Var a) (Fork (Var b) (Fork (Var c) (Var d))))

congruence a b c d = Fork (Atom "SameLength") (fourVar a b c d)

is a b = Fork (Atom "Is") (Fork (Var a) (Var b))

between a b c = Fork
    (Atom "Between")
    (Fork 
        (Var a)
        (Fork (Var b) (Var c))
    )

between2 a b c = Fork
    (Atom "Between")
    (Fork 
        (Var a)
        (Fork b (Var c))
    )


reflexivityOfConguence = congruence "x" "y" "y" "x"
identityOfCongruence = Fork (is "x" "y") (congruence "x" "y" "z" "z")

transitivityOfCongruence = Fork 
    (Fork
        (congruence "z" "u" "v" "w") 
        (congruence "x" "y" "v" "w"))
    (congruence "x" "y" "z" "u")

identityOfBetweenness = Fork (is "x" "y") (between "x" "y" "x")


pachPoint a b c d = Fork (Atom "PointPasch") (fourVar a b c d)
pasch = Fork (Fork (between2 "u" (pachPoint "u" "y" "v" "x") "y") (between "y" "v" "z")) (between "x" "u" "z")

-- Fork inside Atom is not considered implication
testPasch = between2 "u" (pachPoint "u" "y" "v" "x") "y"
testPasch1 = Fork (Fork (Atom "OK") (fourVar "u" "y" "v" "x")) 
    (Fork 
        (Atom "Between")
        (Fork 
            (Var "x")
            (Fork 
            (Var "y")
            (Var "z"))
        ))

twoPoint = Fork (Atom "TwoPoint")  (Fork (Var "y") (Var "x"))

axioms = [
    reflexivityOfConguence,
    identityOfCongruence,
    transitivityOfCongruence,
    identityOfBetweenness,
    pasch,
    testPasch,
    testPasch1,
    twoPoint
    ]
    
