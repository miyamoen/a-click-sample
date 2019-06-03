module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html
import Html.Events
import Json.Decode as JD
import List.Extra
import Time
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always <| Time.every 32 Tick
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , key : Key
    , items : List Item
    , angle : Float
    }


type Msg
    = NoOp
    | ClickedLink UrlRequest
    | UrlChanged Url
    | Tick Time.Posix
    | ToggleRotateItem String


type Route
    = TopRoute
    | ItemRoute String


type alias Item =
    { id : String
    , name : String
    , description : String
    , isRotation : Bool
    , angle : Float
    }


routeparser : Url -> Route
routeparser url =
    UP.parse
        (UP.oneOf
            -- elm reactorで動かしたかった
            [ UP.map ItemRoute (s "Main.elm" </> s "item" </> UP.string)
            , UP.map TopRoute (s "Main.elm")
            ]
        )
        url
        |> Maybe.withDefault TopRoute


itemUrl : String -> String
itemUrl id =
    UB.absolute [ "Main.elm", "item", id ] []


topUrl : String
topUrl =
    UB.absolute [ "Main.elm" ] []


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = routeparser url
      , key = key
      , items =
            [ Item "ham" "ハム" "ハムはおいしいよ" False 0
            , Item "spam" "スパム" "スパムは教養" False 0
            , Item "egg" "たまご" "だし巻き" False 0
            ]
      , angle = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            ( model
            , case urlRequest of
                Internal url ->
                    Nav.pushUrl model.key (Url.toString url)

                External url ->
                    Nav.load url
            )

        UrlChanged url ->
            ( { model | route = routeparser url }
            , Cmd.none
            )

        Tick posix ->
            ( { model
                | angle =
                    Time.posixToMillis posix
                        // 2
                        |> modBy 360
                        |> toFloat
                        |> degrees
              }
            , Cmd.none
            )

        ToggleRotateItem id ->
            ( { model
                | items =
                    List.Extra.updateIf (.id >> (==) id)
                        (\item ->
                            if item.isRotation then
                                { item | isRotation = False, angle = model.angle - item.angle }

                            else
                                { item | isRotation = True, angle = -model.angle }
                        )
                        model.items
              }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "a click sample"
    , body = [ layout [] <| router model ]
    }


router : Model -> Element Msg
router model =
    case model.route of
        TopRoute ->
            topPage model

        ItemRoute id ->
            itemPage id model


topPage : Model -> Element Msg
topPage { items, angle } =
    column [ padding 48, spacing 32, width fill ]
        [ text "Elmでaタグの遷移を止めて別のことをするサンプル"
        , wrappedRow [ spacing 16, width fill ] <|
            List.map (itemCard angle) items
        ]


itemCard : Float -> Item -> Element Msg
itemCard gloabalAngle { id, name, description, isRotation, angle } =
    link
        [ width <| px 300
        , padding 24
        , Border.width 2
        , height fill
        ]
        { url = itemUrl id
        , label =
            column [ spacing 24, height fill ]
                [ el [ Font.size 24 ] <| text name
                , image
                    [ width fill
                    , if isRotation then
                        rotate <| gloabalAngle - angle

                      else
                        rotate angle
                    ]
                    { src = "/img/" ++ id ++ ".png"
                    , description = name ++ "の画像"
                    }
                , paragraph [] [ text description ]
                , el
                    [ Border.rounded 10
                    , paddingXY 16 8
                    , Background.color <| rgb255 255 185 116
                    , preventUrlRequest <| ToggleRotateItem id
                    , pointer
                    , alignBottom
                    , alignRight
                    ]
                  <|
                    if isRotation then
                        text "止める"

                    else
                        text "回す"
                ]
        }


itemPage : String -> Model -> Element Msg
itemPage id model =
    column [ padding 48, spacing 32, width fill ]
        [ link []
            { url = topUrl
            , label = text <| "戻る"
            }
        , case List.Extra.find (.id >> (==) id) model.items of
            Just { name, description, isRotation, angle } ->
                column [ spacing 24 ]
                    [ el [ Font.size 24 ] <| text name
                    , image
                        [ width fill
                        , if isRotation then
                            rotate <| model.angle - angle

                          else
                            rotate angle
                        ]
                        { src = "/img/" ++ id ++ ".png"
                        , description = name ++ "の画像"
                        }
                    , paragraph [] [ text description ]
                    , el
                        [ Border.rounded 10
                        , paddingXY 16 8
                        , Background.color <| rgb255 255 185 116
                        , onClick <| ToggleRotateItem id
                        , pointer
                        , alignBottom
                        , alignRight
                        ]
                      <|
                        if isRotation then
                            text "止める"

                        else
                            text "回す"
                    ]

            Nothing ->
                text <| "id : " ++ id ++ " は見つかりませんでした"
        ]


preventUrlRequest : msg -> Attribute msg
preventUrlRequest msg =
    Html.Events.custom "click"
        (JD.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute
