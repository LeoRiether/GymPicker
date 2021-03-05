module Validate

open FSharp.Data
open Gym

// The Base module defines the logic for the validators
// Most functions here take some parameters and a `gym`, then return
// a boolean checking whether the condition holds or not for the given gym.
// The validators of this module generally shouldn't be used outside of this file,
// as the Stored module wraps them into validators that can be stored in
// the database to cache results.
module Base =
    // Excludes some gym IDs
    let exclude idList gym =
        not (idList |> List.contains gym.Id)

    // Does the contest have a material with the name `tag`?
    // For example, `tag` can be "Tutorial", "Discussion", "Statements", ...
    let hasMaterial (tag: string) acceptedLanguagesOpt (gym: Gym.T) =
        let (ProblemsHtml problemsPage) = gym.Problems.Force()

        let isMaterialWeAreLookingFor (a: HtmlNode) =
            let text = a.InnerText().Trim()
            let startsWithTag = text.StartsWith tag
            let endsWithLanguage =
                match acceptedLanguagesOpt with
                | Some langs ->
                    langs |> List.exists (fun lang -> text.EndsWith ("(" + lang + ")"))
                | None -> true

            startsWithTag && endsWithLanguage

        problemsPage.Descendants ["a"]
        |> Seq.exists isMaterialWeAreLookingFor

    let difficultyBetween low high (gym: Gym.T) =
        gym.Difficulty >= low && gym.Difficulty <= high

    // Check if the contest has standard input and output
    let hasStandardIO (gym: Gym.T) =
        let (Gym.ProblemsHtml html) = gym.Problems.Force()

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
        let (StandingsHtml html) = gym.Standings.Force()

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


//----------------------------------------------------------------------------------------


// Here we wrap the Base validators into StoredValidator records that can
// have the results cached in a database.
// These are the validators you should be using outside of this file!
module Stored =
    open System

    type ValidatorFunction = Gym.T -> bool

    type StoredValidator =
        {
            Fn: ValidatorFunction;
            Key: string;
        }

    type Validator =
        | Simple of ValidatorFunction
        | Stored of StoredValidator
        | OneOf  of Validator list
        | AllOf of Validator list

    let rec run validator gym =
        match validator with
        | Simple fn -> fn gym
        | Stored { Fn = fn; Key = key } ->
            match DB.get key gym.Id with
            | Some cachedResult -> cachedResult
            | None ->
                let res = fn gym
                DB.put key gym.Id res
                res
        | OneOf validators ->
            validators |> List.exists (fun v -> run v gym)
        | AllOf validators ->
            validators |> List.forall (fun v -> run v gym)

    let rec runCached validator gym =
        match validator with
        | Simple fn -> Some (fn gym)
        | Stored { Fn = _; Key = key } -> DB.get key gym.Id

        | OneOf validators ->
            let folder s v =
                match s, runCached v gym with
                // all false -> Some false
                | (Some false, Some false) -> Some false

                // one true -> Some true
                | (_, Some true) | (Some true, _) -> Some true

                // no trues, but one none -> None
                // (we should `run` the validator to know the result for sure)
                | (Some false, None) | (None, _)  -> None

            validators |> List.fold folder (Some false)

        | AllOf validators ->
            let folder s v =
                match s, runCached v gym with
                // all true -> Some true
                | (Some true, Some true) -> Some true

                // one false -> Some false
                | (_, Some false) | (Some false, _) -> Some false

                // no falses, but one none -> None
                // (we should `run` the validator to know the result for sure)
                | (Some true, None) | (None, Some true) | (None, None) -> None

            validators |> List.fold folder (Some true)

    //
    // Combinator validators
    //
    let either a b =
        OneOf [a; b]

    //
    // Some utilities for making validator names
    //
    let join sep (lst: string list) = String.Join(sep, lst)
    let joinBy sep mapping lst =
        lst
        |> List.map mapping
        |> join sep

    //
    // Stored validators
    //
    let exclude idList =
        Simple (Base.exclude idList)

    let hasMaterial (tag: string) acceptedLanguagesOpt =
        let key = sprintf "hasMaterial %s,%A" tag acceptedLanguagesOpt
        Stored { Fn = Base.hasMaterial tag acceptedLanguagesOpt; Key = key }

    let hasTutorial = hasMaterial "Tutorial"
    let hasDiscussion = hasMaterial "Discussion"

    let difficultyBetween low high =
        let key = sprintf "difficultyBetween %d,%d" low high
        Stored { Fn = Base.difficultyBetween low high; Key = key }

    let hasStandardIO =
        Stored { Fn = Base.hasStandardIO; Key = "hasStandardIO" }

    let atLeastNFromTheCountries n countries =
        let key = sprintf "atLeastNFromTheCountries %d,%A" n countries
        Stored { Fn = Base.atLeastNFromTheCountries n countries; Key = key }

    let usersHaveNotParticipated users =
        let key = sprintf "usersHaveNotParticipated %A" users
        Stored { Fn = Base.usersHaveNotParticipated users; Key = key }