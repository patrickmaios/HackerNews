module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time
import Html.Attributes exposing (value)
import Html.Attributes exposing (selected)
import Debug exposing (toString)
import List exposing (sortBy)
import Html.Attributes exposing (checked)
import Model exposing (Model)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config currentTime posts =
  Html.table [] [tableHeader, tableBody config currentTime posts]
    

tableHeader : Html Msg
tableHeader = 
  Html.tr [] 
      [ Html.th [ Html.Attributes.class "post-score"] [ text "score"]
      , Html.th [ Html.Attributes.class "post-title"] [ text "title"]
      , Html.th [ Html.Attributes.class "post-url"] [ text "url"]
      , Html.th [ Html.Attributes.class "post-type"] [ text "type"]
      , Html.th [ Html.Attributes.class "post-time"] [ text "time"]
      ]

tableBody : PostsConfig -> Time.Posix -> List Post -> Html Msg 
tableBody config currentTime posts =
  Html.div [] (List.map (postRow config currentTime) posts) 

postRow : PostsConfig -> Time.Posix -> Post -> Html Msg
postRow config currentTime posts = 
  Html.tr [] 
      [ Html.td [ Html.Attributes.class "post-score"] [ text (String.fromInt posts.score)]
      , Html.td [ Html.Attributes.class "post-title"] [ text posts.title]
      , Html.td [ Html.Attributes.class "post-type"] [ text posts.type_]
      , Html.td [ Html.Attributes.class "post-time"] [ text (Util.Time.formatTime Time.utc posts.time)]
      , Html.td [ Html.Attributes.class "post-url"] [linkView posts.url]
      ]

linkView : Maybe String -> Html Msg
linkView mayburl = 
  case mayburl of 
    Just url -> 
      Html.a [ href (url)] [ text "Link"]
    Nothing -> 
      text "N/A"        


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postPerPageOptions : List Int
postPerPageOptions = [10, 25, 50]

postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
  div []
    [ postsPerPageSelect config
    , sortPostsSelect config
    , showJobPostsCheck config
    , showTextOnlyPostsCheckbox config
    --restu
    ]
  
postsPerPageSelect : PostsConfig -> Html Msg
postsPerPageSelect config =
  let 
    options = List.map (postPerPageOption config) postPerPageOptions
  in
    Html.select [Html.Attributes.id "select-posts-per-page"] options

postPerPageOption : PostsConfig -> Int -> Html Msg
postPerPageOption config options = 
  Html.option [value (String.fromInt options), selected (options == config.postsToShow)] [text (String.fromInt options)]

sortPostsSelect : PostsConfig -> Html Msg
sortPostsSelect config = 
  let
    options = 
      [ ("score", "Score")
      , ("title", "Title")
      , ("date", "Date")
      , ("unsorted", "Unsorted")
      ]
  in    
  div []
    [
    Html.select [Html.Attributes.id "select-sort-by"] (List.map (sortOptions config) options)
    ]

sortOptions : PostsConfig -> (String, String) -> Html Msg
sortOptions config (value, label) = 
  Html.option [ Html.Attributes.value value, selected (value == sortToString config.sortBy)] [text value]

showJobPostsCheck : PostsConfig -> Html Msg
showJobPostsCheck config =
  div [] [
        Html.input [Html.Attributes.id "checkbox-show-job-posts", Html.Attributes.type_ "checkbox", Html.Attributes.checked config.showJobs, Html.Events.onCheck (\x -> ChangeShowJobs x |> ConfigChanged)] []
        ] 

showTextOnlyPostsCheckbox : PostsConfig -> Html Msg
showTextOnlyPostsCheckbox config =
    div [] [
        Html.input [Html.Attributes.id "checkbox-show-text-only-posts", Html.Attributes.type_ "checkbox", checked config.showTextOnly, Html.Events.onCheck (\x -> ChangeShowTextOnly x |> ConfigChanged) ] []
        ]          