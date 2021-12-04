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

between3 a b c = Fork
    (Atom "Between")
    (Fork a (Fork b c))



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
twoPointmade = Fork (Fork (Fork (Atom "TwoPoint")  (Fork (Var "y") (Var "x"))) (Var "y")) (Var "x")


pointLowerDimensionA = Atom "PointLowerDimensionA"
pointLowerDimensionB = Atom "PointLowerDimensionB"
pointLowerDimensionC = Atom "PointLowerDimensionC"
void = Var "void"
lowerDimension1 = Fork void (between3 pointLowerDimensionA pointLowerDimensionB pointLowerDimensionC) 
lowerDimension2 = Fork void (between3 pointLowerDimensionA pointLowerDimensionC pointLowerDimensionA) 
lowerDimension3 = Fork void (between3 pointLowerDimensionC pointLowerDimensionA pointLowerDimensionB) 


orlaw1 = Fork
    (Fork (Fork (Var "d") (Fork (Atom "Or") (Var "b")) ) (Fork (Var "d") (Var "a")))
    (Fork (Var "d") (Fork (Atom "Or") (Fork (Var "a") (Var "b"))))

orlaw2 = Fork (Var "d") (Fork (Var "d") (Fork (Atom "Or") (Atom "Unit")))
orlaw3 = Fork (Fork (Atom "Or") (Fork (Var "a") (Var "b"))) (Fork (Atom "Or") (Var "a"))
unit = Atom "Unit"

orAB = Fork (Atom "Or") (Fork (Var "a") (Var "b"))

orabc a b c = Fork (Atom "Or") (Fork a (Fork b c))
upperDimension = Fork (Fork (Fork (Fork (orabc (between "x" "y" "z") (between "y" "z" "x") (between "z" "x" "y") ) (Fork void (is "u" "v"))) (congruence "z" "u" "z" "v")) (congruence "y" "u" "y" "v")) (congruence "x" "u" "x" "v")

axioms = [
    reflexivityOfConguence,
    identityOfCongruence,
    transitivityOfCongruence,
    identityOfBetweenness,
    pasch,
    pointLowerDimensionA,
    
    pointLowerDimensionB,
    
    pointLowerDimensionC,
    lowerDimension1, lowerDimension2, lowerDimension3,
    upperDimension,
    orlaw1,
    orlaw2,
    orlaw3,
    unit

    ]
    
