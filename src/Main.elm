module Main exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Navigation
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import UrlParser as Url exposing (..)
import Validate exposing (Validator, ifBlank)


-- MAIN


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { keywords : String
    , sellerAccountType : Maybe SellerAccountType
    , currency : Currency
    , minPrice : String
    , maxPrice : String
    , status : Status
    }


initModel : Model
initModel =
    { keywords = ""
    , sellerAccountType = Nothing
    , currency = USD
    , minPrice = ""
    , maxPrice = ""
    , status = NotAsked
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        filters =
            filtersFromLocation location
    in
    { initModel
        | keywords = keywordsFromLoacation location
        , sellerAccountType = sellerAccountTypeFromFilters filters
        , minPrice = priceMinFromFilters filters
        , maxPrice = priceMaxFromFilters filters
        , currency = currencyFromFilters filters
    }
        ! []


type SellerAccountType
    = Individual
    | Business


type Currency
    = USD
    | EUR
    | GBP
    | CAD


type Status
    = NotAsked
    | Asked


keywordsFromLoacation : Navigation.Location -> String
keywordsFromLoacation location =
    location
        |> Url.parseHash (top <?> Url.stringParam "q")
        |> Maybe.map (Maybe.withDefault "")
        |> Maybe.withDefault ""


filtersFromLocation : Navigation.Location -> List (List String)
filtersFromLocation location =
    location
        |> Url.parseHash (top <?> Url.stringParam "filter")
        |> Maybe.map (Maybe.withDefault "")
        |> Maybe.withDefault ""
        |> String.split ","
        |> List.map (String.split ":")


sellerAccountTypeFromFilters : List (List String) -> Maybe SellerAccountType
sellerAccountTypeFromFilters filters =
    filters
        |> List.filter (\f -> List.head f == Just "sellerAccountTypes")
        |> List.head
        |> Maybe.map (\f -> Maybe.withDefault "" (List.head (List.drop 1 f)))
        |> Maybe.withDefault ""
        |> sellerAccountTypeFromString


priceMinFromFilters : List (List String) -> String
priceMinFromFilters filters =
    filters
        |> priceRangeFromFilters
        |> List.head
        |> Maybe.withDefault ""


priceMaxFromFilters : List (List String) -> String
priceMaxFromFilters filters =
    filters
        |> priceRangeFromFilters
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault ""


priceRangeFromFilters : List (List String) -> List String
priceRangeFromFilters filters =
    filters
        |> List.filter (\f -> List.head f == Just "price")
        |> List.head
        |> Maybe.map (\f -> Maybe.withDefault "" (Maybe.map (String.slice 1 -1) (List.head (List.drop 1 f))))
        |> Maybe.withDefault ""
        |> String.split ".."


currencyFromFilters : List (List String) -> Currency
currencyFromFilters filters =
    filters
        |> List.filter (\f -> List.head f == Just "priceCurrency")
        |> List.head
        |> Maybe.map (\f -> Maybe.withDefault "" (List.head (List.drop 1 f)))
        |> Maybe.withDefault ""
        |> currencyFromString


sellerAccountTypesToString : SellerAccountType -> String
sellerAccountTypesToString sellerAccountTypes =
    case sellerAccountTypes of
        Individual ->
            "INDIVIDUAL"

        Business ->
            "BUSINESS"


sellerAccountTypeFromString : String -> Maybe SellerAccountType
sellerAccountTypeFromString str =
    case str of
        "{INDIVIDUAL}" ->
            Just Individual

        "{BUSINESS}" ->
            Just Business

        _ ->
            Nothing


currencyFromString : String -> Currency
currencyFromString str =
    case str of
        "USD" ->
            USD

        "EUR" ->
            EUR

        "GBP" ->
            GBP

        "CAD" ->
            CAD

        _ ->
            USD



-- UPDATE


type Msg
    = UpdKeywords String
    | Search
    | CheckSellerAccountType SellerAccountType
    | CheckCurrency Currency
    | UpdMinPrice String
    | UpdMaxPrice String
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            { model | status = Asked }
                ! [ Navigation.newUrl (generateUrl model) ]

        UpdKeywords str ->
            { model
                | keywords = str
                , status = NotAsked
            }
                ! []

        CheckSellerAccountType str ->
            { model | sellerAccountType = Just str } ! []

        CheckCurrency currency ->
            { model | currency = currency } ! []

        UpdMinPrice str ->
            { model | minPrice = str } ! []

        UpdMaxPrice str ->
            { model | maxPrice = str } ! []

        UrlChange location ->
            model ! []


generateUrl :
    { a
        | keywords : String
        , sellerAccountType : Maybe SellerAccountType
        , currency : Currency
        , minPrice : String
        , maxPrice : String
    }
    -> String
generateUrl model =
    case String.trimLeft model.keywords of
        "" ->
            ""

        keywords ->
            "?q="
                ++ toKeywordsUrl keywords
                ++ toFiltersUrl model.sellerAccountType model.minPrice model.maxPrice model.currency


toKeywordsUrl : String -> String
toKeywordsUrl keywords =
    String.filter (\x -> x /= '*') keywords


toFiltersUrl : Maybe SellerAccountType -> String -> String -> Currency -> String
toFiltersUrl sellerAccountType minPrice maxPrice currency =
    case toFiltersUrl_ sellerAccountType minPrice maxPrice currency of
        "" ->
            ""

        filtersUrl ->
            "&filter="
                ++ filtersUrl


toFiltersUrl_ : Maybe SellerAccountType -> String -> String -> Currency -> String
toFiltersUrl_ sellerAccountType minPrice maxPrice currency =
    [ sellerAccountTypesToUrl sellerAccountType
    , priceRangeToUrl ( minPrice, maxPrice ) currency
    ]
        |> List.filter (String.isEmpty >> not)
        |> List.intersperse ","
        |> List.foldr (++) ""


sellerAccountTypesToUrl : Maybe SellerAccountType -> String
sellerAccountTypesToUrl mSellerAccountType =
    case mSellerAccountType of
        Nothing ->
            ""

        Just sellerAccountType ->
            "sellerAccountTypes:{"
                ++ sellerAccountTypesToString sellerAccountType
                ++ "}"


priceRangeToUrl : ( String, String ) -> Currency -> String
priceRangeToUrl ( mMin, mMax ) currency =
    case priceRangeToString ( mMin, mMax ) of
        "" ->
            ""

        price ->
            price ++ "," ++ priceCurrencyToUrl currency


priceRangeToString : ( String, String ) -> String
priceRangeToString ( mMin, mMax ) =
    case ( toInt mMin, toInt mMax ) of
        ( Just min, Nothing ) ->
            "price:[" ++ min ++ "]"

        ( Nothing, Just max ) ->
            "price:[.." ++ max ++ "]"

        ( Just min, Just max ) ->
            "price:[" ++ min ++ ".." ++ max ++ "]"

        ( Nothing, Nothing ) ->
            ""


priceCurrencyToUrl : Currency -> String
priceCurrencyToUrl currency =
    "priceCurrency:" ++ currencyToString currency


currencyToString : Currency -> String
currencyToString currency =
    case currency of
        USD ->
            "USD"

        EUR ->
            "EUR"

        GBP ->
            "GBP"

        CAD ->
            "CAD"


toInt : String -> Maybe String
toInt =
    String.toInt >> Result.toMaybe >> Maybe.map toString


modelValidator : Validator String Model
modelValidator =
    Validate.all
        [ ifBlank .keywords "Please enter a search query." ]


validate : Model -> Maybe String
validate model =
    case model.status of
        NotAsked ->
            Nothing

        Asked ->
            model
                |> Validate.validate modelValidator
                |> List.head



-- VIEW


view : Model -> Html Msg
view model =
    viewport stylesheet
        (column BodyStyle
            [ height fill, width fill, center ]
            [ header HeaderStyle
                []
                (row
                    Clean
                    [ width fill, center, padding 10 ]
                    [ link "/search-ebay-api/" (el Clean [] (text " Ebay Api Search")) ]
                )
            , mainContent MainContentStyle
                [ height fill, maxWidth (px 1280) ]
                (column Clean
                    [ height fill
                    , paddingRight 15
                    ]
                    [ column Clean [] (searchView model)
                    , row Clean
                        [ height fill ]
                        [ column FiltersStyle
                            [ width (px 300), padding 20, spacing 10 ]
                            [ el Clean [] (text "Filters")
                            , Input.radio CheckSellerStyle
                                [ spacing 10, padding 10 ]
                                { onChange = CheckSellerAccountType
                                , selected = model.sellerAccountType
                                , label = Input.labelAbove (text "Seller Account Typer")
                                , options = []
                                , choices =
                                    [ Input.choice Individual (text "Individual")
                                    , Input.choice Business (text "Business")
                                    ]
                                }
                            , el Clean [] (text "Price")
                            , row Clean
                                [ spacing 10 ]
                                [ Input.text PriceFieldStyle
                                    [ width (px 100), padding 10 ]
                                    { onChange = UpdMinPrice
                                    , value = model.minPrice
                                    , label =
                                        Input.placeholder
                                            { text = "Min"
                                            , label = Input.hiddenLabel ""
                                            }
                                    , options = []
                                    }
                                , Input.text PriceFieldStyle
                                    [ width (px 100), padding 10 ]
                                    { onChange = UpdMaxPrice
                                    , value = model.maxPrice
                                    , label =
                                        Input.placeholder
                                            { text = "Max"
                                            , label = Input.hiddenLabel ""
                                            }
                                    , options = []
                                    }
                                ]
                            , Input.radio CheckCurrencyStyle
                                [ spacing 10, padding 10 ]
                                { onChange = CheckCurrency
                                , selected = Just model.currency
                                , label = Input.labelAbove (text "Currency")
                                , options = []
                                , choices =
                                    [ Input.choice USD (text "USD")
                                    , Input.choice EUR (text "EUR")
                                    , Input.choice GBP (text "GBP")
                                    , Input.choice CAD (text "CAD")
                                    ]
                                }
                            ]
                        , column ListElementsStyles
                            [ width fill, padding 20 ]
                            [ el Clean [] (text "List of elements")
                            ]
                        ]
                    ]
                )
            ]
        )


searchView : Model -> List (Element Styles variation Msg)
searchView model =
    case validate model of
        Just err ->
            [ row Clean
                [ spacing 25
                , paddingLeft 300
                , paddingTop 20
                , paddingBottom 10
                ]
                [ searchFieldView SearchStyle UpdKeywords model.keywords
                , searchButtonView
                ]
            , el ErrorStyle
                [ paddingLeft 310
                , paddingBottom 10
                ]
                (text err)
            ]

        Nothing ->
            [ row Clean
                [ spacing 25
                , paddingLeft 300
                , paddingBottom 20
                , paddingTop 20
                ]
                [ searchFieldView SearchStyle UpdKeywords model.keywords
                , searchButtonView
                ]
            ]


searchButtonView : Element Styles variation Msg
searchButtonView =
    button SeachButtonStyle
        [ paddingXY 50 20, onClick Search ]
        (text "Search")


searchFieldView : Styles -> (String -> msg) -> String -> Element Styles variation msg
searchFieldView style message keywords =
    Input.search style
        [ padding 20 ]
        { onChange = message
        , value = keywords
        , label =
            Input.placeholder
                { text = "Search for anything"
                , label = Input.hiddenLabel ""
                }
        , options = []
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- LAYOUT


type Styles
    = Clean
    | HeaderStyle
    | SearchStyle
    | SearchStyleNotValid
    | CheckSellerStyle
    | FiltersStyle
    | MainContentStyle
    | BodyStyle
    | ListElementsStyles
    | CheckCurrencyStyle
    | PriceFieldStyle
    | SeachButtonStyle
    | ErrorStyle


stylesheet =
    Style.styleSheet
        [ style Clean []
        , style HeaderStyle
            [ Color.background lightBlue
            , Color.border blue
            , Border.bottom 2
            ]
        , style SearchStyle
            [ Border.all 1
            , Color.border lightGrey
            ]
        , style SearchStyleNotValid
            [ Border.all 1
            , Color.border red
            ]
        , style FiltersStyle
            [ Color.background grey
            , Color.border green
            ]
        , style BodyStyle
            [ Color.background lightGrey ]
        , style ListElementsStyles
            [ Color.background white
            , Color.border green
            , Border.left 2
            , Border.top 2
            ]
        , style PriceFieldStyle
            [ Border.all 1
            , Color.border black
            ]
        , style SeachButtonStyle
            [ Color.background blue
            , Color.text white
            ]
        , style ErrorStyle
            [ Color.text red ]
        , style MainContentStyle
            []
        , style CheckSellerStyle
            []
        , style CheckCurrencyStyle
            []
        ]
