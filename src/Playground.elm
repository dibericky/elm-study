module Playground exposing (Character, Greeting(..), getAdultAge, main, sayHello)

import Html
import MyList
import Regex


escapeEarth myVelocity mySpeed fuelStatus =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitalSpeedInKmPerSec =
            7.67

        whereToLand =
            if fuelStatus == "low" then
                "Land on droneship"

            else
                "Land on launchpad"
    in
    if myVelocity > escapeVelocityInKmPerSec then
        "Godspeed"

    else if mySpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"

    else
        whereToLand


computeSpeed distance time =
    distance / time


computeTime startTime endTime =
    endTime - startTime


add a b =
    a + b


multiply c d =
    c * d


divide e f =
    e / f



{-
   main =
       divide 30 10
           |> multiply 10
           |> add 5
           |> String.fromFloat
           |> Html.text
-}


weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown day"


hashtag dayInNumber =
    case weekday dayInNumber of
        "Sunday" ->
            "#SinDay"

        "Monday" ->
            "#MondayBlues"

        "Tuesday" ->
            "#TakeMeBackTuesday"

        "Wednesday" ->
            "#HumpDay"

        "Thursday" ->
            "#ThrowbackThursday"

        "Friday" ->
            "#FlashbackFriday"

        "Saturday" ->
            "#Caturday"

        _ ->
            "#Whatever"


isNotDash char =
    char /= '-'


removeDash string =
    String.filter isNotDash string


removeSpace string =
    String.filter (\char -> char /= ' ') string


revelation =
    """
    It became very clear to me sitting out there today
    that every decision I've made in my entire life has
    been wrong. My life is the complete "opposite" of
    everything I want it to be. Every instinct I have,
    in every aspect of life, be it something to wear,
    something to eat - it's all been wrong.
    """


apolloExercise =
    let
        pattern =
            "\\d\\d:\\d\\d (a\\.m\\.|p\\.m\\.)"

        regex =
            Regex.fromStringWith { caseInsensitive = True, multiline = False } pattern
                |> Maybe.withDefault Regex.never

        apollo11 =
            """ 
        On July 16, 1969, the massive Saturn V rocket 
        lifted off from NASA's Kennedy Space Center at 
        09:32 a.m. EDT. Four days later, on July 20, Neil 
        Armstrong and Buzz Aldrin landed on the Moon. 
        """

        launchTimes =
            Regex.find regex apollo11

        foundTimes =
            List.map (\launchTime -> launchTime.match) launchTimes
    in
    foundTimes


isEvil name =
    List.member name [ "Joffrey", "Ramsey" ]


partitionated =
    List.partition isEvil [ "Samwell", "Joffrey", "Hodor", "Ramsay" ]


descending a b =
    case compare a b of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


evilometer character1 character2 =
    case ( character1, character2 ) of
        ( "Joffrey", "Ramsay" ) ->
            LT

        ( "Joffrey", "Night King" ) ->
            LT

        ( "Ramsay", "Joffrey" ) ->
            GT

        ( "Ramsay", "Night King" ) ->
            LT

        ( "Night King", "Joffrey" ) ->
            GT

        ( "Night King", "Ramsay" ) ->
            GT

        _ ->
            GT


initialValue =
    0


funToReduce item acc =
    item + acc


sumAll list =
    List.foldl funToReduce initialValue list


type alias TVShow =
    { creator : String, episodes : Int, name : String }


firefly =
    TVShow "Joss Whedon" 14 "Firefly"


fringe =
    TVShow "J.J. Abrams" 100 "Fringe"


got =
    TVShow "" 60 "Game of Thrones"


hasCreaor tvShow =
    String.length tvShow.creator > 0


sortedByEpisode =
    List.sortBy .episodes [ fringe, firefly, got ]


incrementEpisode tvShow =
    { tvShow | episodes = tvShow.episodes + 1 }


changeNameAndCreator tvShow newName newCreator =
    { tvShow
        | name = newName
        , creator = newCreator
    }


addOne : number -> number
addOne y =
    y + 1



{-
   main =
       [ "Night King", "Joffrey", "Ramsay" ]
           |> List.map (String.contains " ")
           |> Debug.toString
           |> Html.text
-}


type Greeting
    = Howdy
    | Hola
    | Namaste String
    | NumericalHi Int Int


sayHello : Greeting -> String
sayHello greeting =
    case greeting of
        Howdy ->
            "How y'all doin'?"

        Hola ->
            "Hola amigo!"

        Namaste message ->
            message

        NumericalHi value1 value2 ->
            value1 + value2 |> String.fromInt


signUp : String -> String -> Result String String
signUp email ageStr =
    case String.toInt ageStr of
        Nothing ->
            Err "Age must be an integer."

        Just age ->
            let
                emailPattern =
                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

                regex =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString emailPattern

                isValidEmail =
                    Regex.contains regex email
            in
            if age < 13 then
                Err "You need to be at least 13 years old to sign up."

            else if isValidEmail then
                Ok "Your account has been created successfully!"

            else
                Err "You entered an invalid email."


type alias Character =
    { name : String
    , age : Maybe Int
    }


sansa : Character
sansa =
    { name = "Sansa"
    , age = Just 19
    }


arya : Character
arya =
    { name = "Arya"
    , age = Nothing
    }


getAdultAge : Character -> Maybe Int
getAdultAge character =
    case character.age of
        Nothing ->
            Nothing

        Just age ->
            if age >= 18 then
                Just age

            else
                Nothing


list1 : MyList.MyList a
list1 =
    MyList.Empty


list2 : MyList.MyList number
list2 =
    MyList.Node 9 MyList.Empty


main : Html.Html msg
main =
    MyList.isEmpty list2
        |> Debug.toString
        |> Html.text



{-
   main =
       escapeEarth 10 6.7 "low"
           |> Html.text
-}
{- main =
   Html.text (String.fromFloat(add 5 (multiply 10 (divide 30 10))))
-}
{- main =
   computeTime 2 3
       |> computeSpeed 7.67
       |> escapeEarth 11
       |> Html.text
-}
