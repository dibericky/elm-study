module FuzzTests exposing (addOneTests, addTests, flipTests, multiplyFloatTests, pizzaLeftTests, stringTests)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (..)
import Random exposing (maxInt, minInt)
import Test exposing (..)


addOne : Int -> Int
addOne x =
    x + 1


addOneTests : Test
addOneTests =
    describe "addOne"
        [ fuzz frequencyFuzzer "adds 1 to any integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        ]


add : Int -> Int -> Int
add x y =
    x + y


addTests : Test
addTests =
    describe "add"
        [ fuzz2 int int "add two givent integers" <|
            \num1 num2 ->
                add num1 num2
                    |> Expect.equal (num1 + num2)
        ]


flip : Bool -> Bool
flip x =
    not x


flipTests : Test
flipTests =
    describe "flip"
        [ fuzz bool "negates the given boolean value" <|
            \value ->
                flip value |> Expect.equal (not value)
        ]


multiplyFloat : Float -> Int -> Float
multiplyFloat x y =
    x * toFloat y


multiplyFloatTests : Test
multiplyFloatTests =
    describe "multiplyFloat"
        [ fuzz2 float int "multiplies given numbers" <|
            \x y ->
                multiplyFloat x y
                    |> Expect.within (Absolute 0.000000001) (x * toFloat y)
        ]


pizzaLeft : Float -> Float -> Float
pizzaLeft eatenPercent totalSlices =
    totalSlices - (eatenPercent * totalSlices)


pizzaLeftTests : Test
pizzaLeftTests =
    describe "pizzaLeft"
        [ fuzz2 percentage float "returns remaining pizza slices" <|
            \eaten total ->
                pizzaLeft eaten total
                    |> Expect.within (Absolute 0.000000001) (total - (eaten * total))
        ]


stringTests : Test
stringTests =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                -- Unit Test - 1
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    palindrome
                        |> String.reverse
                        |> Expect.equal palindrome
            , test "reverses a known string" <|
                -- Unit Test - 2
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , fuzz string "restores the original string if you run it again" <|
                -- Fuzz Test
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


frequencyFuzzer : Fuzzer Int
frequencyFuzzer =
    frequency
        [ ( 70, constant 7 )
        , ( 12, intRange 8 9 )
        , ( 6, constant 6 )
        , ( 9, intRange 2 4 )
        , ( 1, constant 5 )
        , ( 1, constant 1 )
        , ( 1, constant 10 )
        ]
