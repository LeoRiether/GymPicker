module Validate

open FSharp.Data
open Gym

type Validator =
    | APIValidator of (Gym.T -> bool) // validates the json data from the API
    | ContestValidator of (HtmlDocument -> bool) // validates the contest page
    | StandingsValidator of (HtmlDocument -> bool) // validates the json data from the API
    | OrValidator of Validator * Validator

let rec run
        gym
        (contest: Lazy<HtmlDocument>)
        (standings: Lazy<HtmlDocument>)
    = function
    | APIValidator f -> f gym
    | ContestValidator f -> f (contest.Force())
    | StandingsValidator f -> f (standings.Force())
    | OrValidator (a, b) -> (run gym contest standings a) || (run gym contest standings b)

let either a b =
    OrValidator (a, b)

// Asserts all validators pass
let forall
    validators
    gym
    (contest: Lazy<HtmlDocument>)
    (standings: Lazy<HtmlDocument>)
    =
    List.forall (run gym contest standings) validators

// Does the contest have a material with the name `tag`?
// For example, `tag` can be "Tutorial", "Discussion", "Statements", ...
let hasMaterial (tag: string) acceptedLanguagesOpt =
    let inner (html: HtmlDocument) =
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

    ContestValidator inner

// Does the gym have a tutorial in one of the accepted languages?
let hasTutorial = hasMaterial "Tutorial"
let hasDiscussion = hasMaterial "Discussion"

let difficultyBetween low high =
    APIValidator (fun gym -> gym.Difficulty >= low && gym.Difficulty <= high)

// Check if the contest has standard input and output
let hasStandardIO =
    let inner (html: HtmlDocument) =
        html.CssSelect "table.problems .notice"
        |> List.forall (fun node -> node.InnerText().Contains("standard input/output"))

    ContestValidator inner

// Checks if at least N teams from a list of countries have participated
// Each country string should be listed as the two-character abbreviation of the country,
// like "br" for Brazil, "cn" for China, "us" for United States, etc. If you're not sure what
// abbreviation to use, go a standings page and open the flag image for your country in a new tab.
// The image should be named `{the two-character abbreviation of your country}.png`
let atLeastNFromTheCountries n countries =
    // Is the flag from one of the countries in `countries`?
    let checkFlag (img: HtmlNode) =
        countries
        |> List.exists (fun country ->
            img.AttributeValue("src").EndsWith(country + ".png")
        )

    let inner (html: HtmlDocument) =
        let count =
            html.CssSelect "img.standings-flag"
            |> Seq.filter checkFlag
            |> Seq.length

        count >= n

    StandingsValidator inner