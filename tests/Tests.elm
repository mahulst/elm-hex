module Tests exposing (..)

import Test exposing (describe, test, fuzz3, Test)
import Fuzz exposing (int)
import Expect
import String
import Array exposing (Array)
import Main exposing (Position, Cube)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


array : Array Int
array =
    Array.fromList [ 0, 1, 3, 5, 7, 11, 0, 13, 17, 0, 0 ]


all : Test
all =
    describe "Cube logic"
        [ describe "Cube to position"
            [ test "0 0 0" <|
                \() ->
                    let
                        cube =
                            Cube 0 0 0

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position 0 0
                    in
                        Expect.equal hex expectedHex
            , test "east" <|
                \() ->
                    let
                        cube =
                            Cube 1 -1 0

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position 1 0
                    in
                        Expect.equal hex expectedHex
            , test "southEast" <|
                \() ->
                    let
                        cube =
                            Cube 0 -1 1

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position 0 1
                    in
                        Expect.equal hex expectedHex
            , test "southWest" <|
                \() ->
                    let
                        cube =
                            Cube -1 0 1

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position -1 1
                    in
                        Expect.equal hex expectedHex
            , test "west" <|
                \() ->
                    let
                        cube =
                            Cube -1 1 0

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position -1 0
                    in
                        Expect.equal hex expectedHex
            , test "northWest" <|
                \() ->
                    let
                        cube =
                            Cube 0 1 -1

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position -1 -1
                    in
                        Expect.equal hex expectedHex
            , test "nortEast" <|
                \() ->
                    let
                        cube =
                            Cube 1 0 -1

                        hex =
                            Main.cubeToOddr cube

                        expectedHex =
                            Position 0 -1
                    in
                        Expect.equal hex expectedHex
            , fuzz3 int int int "restores original cube 1" <|
                \x y z ->
                    (Cube x y (0 - (x + y)))
                        |> Main.cubeToOddr
                        |> Main.oddrToCube
                        |> Expect.equal (Cube x y (0 - (x + y)))
            , fuzz3 int int int "restores original cube 2" <|
                \x y z ->
                    (Cube x (0 - (x + z)) z)
                        |> Main.cubeToOddr
                        |> Main.oddrToCube
                        |> Expect.equal (Cube x (0 - (x + z)) z)
            , fuzz3 int int int "restores original cube 3" <|
                \x y z ->
                    (Cube (0 - (z + y)) y z)
                        |> Main.cubeToOddr
                        |> Main.oddrToCube
                        |> Expect.equal (Cube (0 - (z + y)) y z)
            ]
        ]
