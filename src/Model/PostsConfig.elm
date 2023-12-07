module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString, defaultConfig)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" -> Just Score
        "Title" -> Just Title
        "Posted" -> Just Posted
        "None" -> Just None
        _ -> Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postB.title postA.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = ChangePostsToFetch Int
    | ChangePostsToShow Int
    | ChangeSortBy SortBy
    | ChangeShowJobs Bool
    | ChangeShowTextOnly Bool


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        ChangePostsToFetch val -> 
            {config | postsToFetch = val}

        ChangePostsToShow val ->
            {config | postsToShow = val}

        ChangeSortBy val ->
            {config | sortBy = val}

        ChangeShowJobs val ->
            {config | showJobs = val}

        ChangeShowTextOnly val ->
            {config | showTextOnly = val}

    


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}

filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        -- Filter text-only posts if showTextOnly is False
        --Postare fara url!!!!!!!!!!!!!!!
        filteredTextPosts =
            if config.showTextOnly then
                posts
            else
                List.filter (\post -> post.url /= Nothing) posts

        -- Filter job posts if showJobs is False
        filteredJobPosts =
            if config.showJobs then
                filteredTextPosts
            else
                List.filter (\post -> post.type_ /= "job") filteredTextPosts

        -- Sort the posts according to sortBy
        limitedPosts =
            List.take config.postsToShow filteredJobPosts

        -- Take at most postsToShow posts
        sortedPosts =
            List.sortWith (sortToCompareFn config.sortBy) limitedPosts
    
    in
        sortedPosts