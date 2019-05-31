module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Element exposing (..)
import Html
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , key : Key
    , items : List Item
    }


type Msg
    = NoOp
    | ClickedLink UrlRequest
    | UrlChanged Url


type Route
    = TopRoute
    | ItemRoute String


type alias Item =
    { id : String }


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
      , items = [ Item "Ham", Item "Spam", Item "Egg" ]
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


view : Model -> Document Msg
view model =
    { title = "a click sample"
    , body = [ layout [] <| router model ]
    }


router : Model -> Element msg
router model =
    case model.route of
        TopRoute ->
            column []
                [ text "top"
                , link []
                    { url = itemUrl "Ham"
                    , label = text <| "item link Ham"
                    }
                ]

        ItemRoute id ->
            column []
                [ text <| "item : " ++ id
                , link []
                    { url = topUrl
                    , label = text <| "return to Top"
                    }
                ]
