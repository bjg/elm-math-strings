module Math.Strings.TokensTests exposing (operatorFuzzer, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, char, int, list, string, tuple)
import Math.Strings.Tokens as Tokens exposing (Token(..))
import Test exposing (..)


operatorFuzzer =
    [ '(', ')', '+', '-', '*', '/' ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


suite : Test
suite =
    describe "Tokens"
        [ describe "parseToken"
            [ test "interprests spaces as whitespace tokens" <|
                \_ ->
                    Tokens.parseToken ' '
                        |> Expect.equal (Ok Whitespace)
            , fuzz (Fuzz.intRange 0 9) "interprets numbers as digit tokens" <|
                \i ->
                    let
                        iAsChar =
                            String.fromInt i
                                |> String.uncons
                                |> Maybe.map (\( h, t ) -> h)
                                |> Maybe.withDefault '0'
                    in
                    Tokens.parseToken iAsChar
                        |> Expect.equal (Ok (Digit iAsChar))
            , fuzz operatorFuzzer "interprets operators as Operators" <|
                \c ->
                    Tokens.parseToken c
                        |> (\token ->
                                case token of
                                    Ok (Op _) ->
                                        Expect.pass

                                    _ ->
                                        String.fromChar c
                                            ++ " is not an operator"
                                            |> Expect.fail
                           )
            ]
        ]
