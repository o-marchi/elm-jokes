module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    , getNewJoke
    )


jokeDecoder : Decoder Joke
jokeDecoder =
    map4 Joke
        (field "type" string)
        (field "setup" string)
        (field "punchline" string)
        (succeed False)


getNewJoke : Cmd Msg
getNewJoke =
    Http.get
        { url = "https://official-joke-api.appspot.com/random_joke"
        , expect = Http.expectJson GotData jokeDecoder
        }



---- UPDATE ----


type Msg
    = GetNewJoke
    | GotData (Result Http.Error Joke)
    | Reveal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewJoke ->
            ( { model | loading = True } , getNewJoke)

        GotData (Ok data) ->
            ( { model
                | joke = data
                , loading = False
              }
            , Cmd.none
            )

        GotData (Err error) ->
            ( { model | loading = False }, Cmd.none )

        Reveal ->
            let
              oldjoke = model.joke
            in
            ( { model | joke =
                  { oldjoke | showpunchline = True }
              }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "container" ]
            [ if model.loading then
                div [ class "loader" ]
                  [ span [] [ text "{" ]
                  , span [] [ text "}" ]
                  ]

              else
                div []
                  [ renderJoke model.joke
                  , button [ onClick GetNewJoke
                           , class "button button--secondary"
                           ]
                      [ span [ class "button__inner" ]
                          [ text "Another one" ]
                      ]
                  ]
            ]
        ]

renderJoke : Joke -> Html Msg
renderJoke joke =
    div [ class "joke" ]
        [ h1 [] [ text joke.setup ]
        , div [ class "punchline" ]
          [ if joke.showpunchline then
            p [] [ text joke.punchline ]

          else
            span [ onClick Reveal ] [ text "Reveal" ]
          ]
        ]
