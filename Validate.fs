module Validate

open FSharp.Data
open Gym

let either a b gym =
    (a gym) || (b gym)

// Asserts all validators pass
let forall validators gym =
    List.forall (fun f -> f gym) validators

// Excludes some gym IDs
let exclude idList gym =
    not (idList |> List.contains gym.Id)

// Does the contest have a material with the name `tag`?
// For example, `tag` can be "Tutorial", "Discussion", "Statements", ...
let hasMaterial (tag: string) acceptedLanguagesOpt (gym: Gym.T) =
    let (Gym.ContestHtml html) = gym.Contest.Force()

    let isTutorialAnchor (a: HtmlNode) =
        let text = a.InnerText().Trim()
        let startsWithTutorial = text.StartsWith tag
        let endsWithLanguage =
            match acceptedLanguagesOpt with
            | Some langs ->
                langs |> List.exists (fun lang -> text.EndsWith ("(" + lang + ")"))
            | None -> true

        startsWithTutorial && endsWithLanguage

    html.Descendants ["a"]
    |> Seq.tryFind isTutorialAnchor
    |> Option.isSome

// Does the gym have a tutorial in one of the accepted languages?
let hasTutorial = hasMaterial "Tutorial"
let hasDiscussion = hasMaterial "Discussion"

let difficultyBetween low high (gym: Gym.T) =
    gym.Difficulty >= low && gym.Difficulty <= high

// Check if the contest has standard input and output
let hasStandardIO (gym: Gym.T) =
    let (Gym.ContestHtml html) = gym.Contest.Force()

    html.CssSelect "table.problems .notice"
    |> List.forall (fun node -> node.InnerText().Contains("standard input/output"))

// Checks if at least N teams from a list of countries have participated
// Each country string should be listed as the two-character abbreviation of the country,
// like "br" for Brazil, "cn" for China, "us" for United States, etc. If you're not sure what
// abbreviation to use, go a standings page and open the flag image for your country in a new tab.
// The image should be named `{the two-character abbreviation of your country}.png`
let atLeastNFromTheCountries n countries (gym: Gym.T) =
    let (Gym.StandingsHtml html) = gym.Standings.Force()

    // Is the flag from one of the countries in `countries`?
    let checkFlag (img: HtmlNode) =
        countries
        |> List.exists (fun country ->
            img.AttributeValue("src").EndsWith(country + ".png")
        )

    let count =
        html.CssSelect "img.standings-flag"
        |> Seq.filter checkFlag
        |> Seq.length

    count >= n

// Assert all users in the standings pass the predicate
let usersHaveProperty pred gym =
    let (Gym.StandingsHtml html) = gym.Standings.Force()

    let extractUsername(a: HtmlNode) =
        let href = a.AttributeValue("href")
        if href.StartsWith "/profile/"
            then Some (href.Substring 9) // remove /profile/ from the start
            else None

    html.CssSelect "table.standings .contestant-cell a"
    |> Seq.choose extractUsername // choose is filterMap
    |> Seq.forall pred

let usersHaveNotParticipated users =
    usersHaveProperty (fun u -> not (List.contains u users))

// TODO: implement this
// let usersHaveParticipated users =
//     usersHaveProperty (fun u -> List.contains u users)