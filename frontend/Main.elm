module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random

import Anagrams exposing (..)
import Game exposing (..)

type alias Model =
  { currentAnagram : Anagram
  , currentWord : String
  , submissions : List String
  , state : GameState
  }

type Msg = CharacterClicked Char
         | ClearWord
         | NewGame
         | GiveUp
         | RandomAnagram
         | NewAnagram Anagram

init =
  (Model (Anagram [] []) "" [] Menu, Cmd.none)

buttons anagram =
  List.map (\c -> button [ onClick <| CharacterClicked c ] [ text <| String.fromChar c ]) anagram.characters

view model =
  div []
    [ Html.node "link" [ rel "stylesheet", href "style.css" ] []
    , Html.node "meta" [ name "viewport", content "width=device-width, initial-scale=1.0" ] []
    , case model.state of
        Menu ->
          viewMenu
        Playing ->
          viewPlaying model
        Won ->
          viewGameWon
    ]

viewMenu =
  div []
    [ div [] [ text "Menu" ]
    , button [ onClick NewGame ] [ text "New Game" ]
    ]

viewGameWon =
  div []
    [ div [] [ text "Won!" ]
    , button [ onClick NewGame ] [ text "New Game" ]
    ]

viewPlaying model =
  div []
    [ div [ class "words" ]
        [ div [] (List.map (\submission -> div [] [ text submission ]) model.submissions)
        , div [] (List.map (\missingWord -> div [] [ text missingWord ]) <| missingWords model)
        ]
    , div [ class "currentWord" ] [ text <| if String.length model.currentWord > 0 then model.currentWord else "..." ]
    , div [ class "characterButtons" ] <| buttons model.currentAnagram
    , button [ onClick ClearWord ] [ text "Clear" ]
    , button [ onClick GiveUp ] [ text "Give up" ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CharacterClicked c ->
      let
        newWord = String.append model.currentWord (String.fromChar c)
        solutionFound = (isSolution model.currentAnagram newWord) && (not <| List.member newWord model.submissions)
        newSubmissions = if solutionFound then newWord :: model.submissions else model.submissions
        newerWord = if solutionFound then "" else newWord
        newGameWon = if gameDone model.currentAnagram.solutions newSubmissions then Won else Playing
      in
        ({ model | currentWord = newerWord, submissions = newSubmissions, state = newGameWon }, Cmd.none)

    ClearWord ->
      ({ model | currentWord = "" }, Cmd.none)

    NewGame ->
      let
        ( model, cmd ) = init
      in
        ( { model | state = Playing }, Random.generate NewAnagram randomAnagram )

    GiveUp ->
      ( { model | state = Menu }, Cmd.none )

    RandomAnagram ->
      ( model, Random.generate NewAnagram randomAnagram )

    NewAnagram anagram ->
      ( { model | currentAnagram = anagram }, Cmd.none )

missingWords model =
  List.map (\word -> String.fromList <| List.repeat (String.length word) '-') <| List.filter (\solution -> not <| List.member solution model.submissions) model.currentAnagram.solutions

isSolution anagram word =
  List.member word <| List.map (\solution -> String.toLower solution) anagram.solutions

gameDone solutions submissions =
  List.length solutions == List.length submissions

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }
