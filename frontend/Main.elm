module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Anagrams exposing (..)

type alias Model =
  { currentAnagram : Anagram
  , currentWord : String
  , gameWon : Bool
  , submissions : List String
  }

type Msg = CharacterClicked Char
         | ClearWord
         | NewGame

init =
  (Model randomAnagram "" False [], Cmd.none)

buttons anagram =
  List.map (\c -> button [ onClick <| CharacterClicked c ] [ text <| String.fromChar c ]) anagram.characters

view model =
  div []
    [ Html.node "link" [ rel "stylesheet", href "style.css" ] []
    , Html.node "meta" [ name "viewport", content "width=device-width, initial-scale=1.0" ] []
    , if model.gameWon then
        div []
          [ div [] [ text "Won!" ]
          , button [ onClick NewGame ] [ text "New Game" ]
          ]
      else
        div []
          [ div [ class "words" ]
              [ div [] (List.map (\submission -> div [] [ text submission ]) model.submissions)
              , div [] (List.map (\missingWord -> div [] [ text missingWord ]) <| missingWords model)
              ]
          , div [ class "currentWord" ] [ text <| if String.length model.currentWord > 0 then model.currentWord else "..." ]
          , div [ class "characterButtons" ] <| buttons model.currentAnagram
          , button [ onClick ClearWord ] [ text "Clear" ]
          ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CharacterClicked c ->
      let
        newWord = String.append model.currentWord (String.fromChar c)
        solutionFound = isSolution model.currentAnagram newWord
        newSubmissions = if solutionFound then newWord :: model.submissions else model.submissions
        newerWord = if solutionFound then "" else newWord
        newGameWon = gameDone model.currentAnagram.solutions newSubmissions
      in
        ({ model | currentWord = newerWord, submissions = newSubmissions, gameWon = newGameWon }, Cmd.none)

    ClearWord ->
      ({ model | currentWord = "" }, Cmd.none)

    NewGame ->
      init

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
