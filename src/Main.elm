module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, field, int, map3, map4, string, succeed)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Joke =
    { category : String
    , setup : String
    , punchline : String
    , showpunchline : Bool
    }


type alias Model =
    { joke : Joke
    , loading : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { joke =
            { category = "Oioi"
            , setup = "Who?"
            , punchline = "me"
            , showpunchline = False
            }
      , loading = True
      }
    , Http.get
        { url = "https://official-joke-api.appspot.com/random_joke"
        , expect = Http.expectJson GotData jokeDecoder
        }
    )


jokeDecoder : Decoder Joke
jokeDecoder =
    map4 Joke
        (field "type" string)
        (field "setup" string)
        (field "type" string)
        (succeed False)



---- UPDATE ----


type Msg
    = GotData (Result Http.Error Joke)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok data) ->
            let
                log =
                    Debug.log "data" data
            in
            ( { model
                | joke = data
                , loading = False
              }
            , Cmd.none
            )

        GotData (Err error) ->
            ( { model | loading = False }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "container" ]
            [ if model.loading then
                p [ class "loading" ] [ text "Loading" ]

              else
                renderJoke model.joke
            ]
        , button [] [ text "Another joke" ]
        ]


renderJoke : Joke -> Html Msg
renderJoke joke =
    div [ class "joke" ]
        [ h4 [] [ text joke.setup ]
        , if joke.showpunchline then
            p [] [ text joke.punchline ]

          else
            button [] [ text "Reveal" ]
        ]
