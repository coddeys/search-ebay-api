module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


urlGeneration : Test
urlGeneration =
    describe "The Url Generation"
        [ describe "generateUrl"
            [ test "generate an empty url if no keywords" <|
                \_ ->
                    Expect.equal "" (generateUrl empty)
            , test "first 3 symbols if not empty" <|
                \_ ->
                    generateUrl notEmpty
                        |> String.slice 0 3
                        |> Expect.equal "?q="
            , test "fullExample" <|
                \_ ->
                    generateUrl fullExample
                        |> Expect.equal "?q=phone samsung&filter=sellerAccountTypes:{INDIVIDUAL},price:[50..500],priceCurrency:USD"
            ]
        , describe "toKeywordsUrl"
            [ test "remove * symbol" <|
                \_ ->
                    Expect.equal "test" (toKeywordsUrl "test*")
            , fuzz string "fuzz remove * symbol" <|
                \randomlyGeneratedString ->
                    toKeywordsUrl randomlyGeneratedString
                        |> Expect.equal (String.filter (\x -> x /= '*') randomlyGeneratedString)
            ]
        , describe "toFiltersUrl"
            [ test "generate an empty string if nothing checked" <|
                \_ ->
                    toFiltersUrl Nothing "" "" USD
                        |> Expect.equal ""
            , test "first 8 symbols if not empty" <|
                \_ ->
                    toFiltersUrl (Just Individual) "" "" USD
                        |> String.slice 0 8
                        |> Expect.equal "&filter="
            ]
        , describe "sellerAccountTypesToUrl"
            [ test "generate an empty string if nothing checked" <|
                \_ ->
                    sellerAccountTypesToUrl Nothing
                        |> Expect.equal ""
            , test "check url for individual" <|
                \_ ->
                    sellerAccountTypesToUrl (Just Individual)
                        |> Expect.equal "sellerAccountTypes:{INDIVIDUAL}"
            , test "check url for Business" <|
                \_ ->
                    sellerAccountTypesToUrl (Just Business)
                        |> Expect.equal "sellerAccountTypes:{BUSINESS}"
            ]
        , describe "priceRangeToString"
            [ test "generate an empty string if no input" <|
                \_ ->
                    priceRangeToString ( "", "" )
                        |> Expect.equal ""
            , test "generate an empty string if inputs are invalid" <|
                \_ ->
                    priceRangeToString ( "asdf", "asdf" )
                        |> Expect.equal ""
            , test "if only min" <|
                \_ ->
                    priceRangeToString ( "10", "" )
                        |> Expect.equal "price:[10]"
            , test "if only max" <|
                \_ ->
                    priceRangeToString ( "", "10" )
                        |> Expect.equal "price:[..10]"
            , test "if min and max" <|
                \_ ->
                    priceRangeToString ( "10", "200" )
                        |> Expect.equal "price:[10..200]"
            ]
        , describe "priceCurrencyToUrl"
            [ test "generate currency USD" <|
                \_ ->
                    priceCurrencyToUrl USD
                        |> Expect.equal "priceCurrency:USD"
            , test "generate currency EUR" <|
                \_ ->
                    priceCurrencyToUrl EUR
                        |> Expect.equal "priceCurrency:EUR"
            ]
        ]


empty =
    { keywords = ""
    , sellerAccountType = Nothing
    , currency = USD
    , minPrice = ""
    , maxPrice = ""
    }


notEmpty =
    { keywords = "notEmpty"
    , sellerAccountType = Nothing
    , currency = USD
    , minPrice = ""
    , maxPrice = ""
    }


fullExample =
    { keywords = "phone samsung"
    , sellerAccountType = Just Individual
    , currency = USD
    , minPrice = "50"
    , maxPrice = "500"
    }
