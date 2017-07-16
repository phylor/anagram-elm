module Anagrams exposing (..)

import Random exposing (Generator)

type alias Anagram =
  { characters : List Char
  , solutions : List String
  }

anagrams =
  [ Anagram [ 'e', 'i', 'l', 's', 't' ] [ "Liste", "Stiel", "Stile", "eilst", "liest", "liste", "seilt", "steil", "teils" ]
  , Anagram [ 'e', 'i', 's' ] [ "eis", "sie", "sei", "sie" ]
  , Anagram [ 'e', 'e', 'f', 'i', 'n', 'r' ] [ "Ferien", "Reifen", "eifern", "feiern", "feiner", "freien", "reifen", "riefen" ]
  , Anagram [ 'e', 'e', 'i', 'n', 'r', 's' ] [ "Eierns", "Einser", "Irenes", "Reisen", "Riesen", "Serien", "Sirene", "eisern", "reines", "reisen", "seiner" ]
  , Anagram [ 'a', 'e', 'g', 'n', 'r' ] [ "Anger", "Garne", "Nager", "argen", "garen", "ragen" ]
  ]

randomAnagram : Generator Anagram
randomAnagram =
  Random.map (\index -> getAnagram index) (Random.int 0 (List.length anagrams - 1))

getAnagram index =
  Maybe.withDefault (Anagram [] []) <| List.head <| List.drop index anagrams
 
