module Validate

open FSharp.Data
open Gym

type Validator =
    | HtmlValidator of (HtmlDocument -> bool) // validates the contest page
    | JsonValidator of (Gym.T -> bool) // validates the json data from the API

// Runs a sequence of validators
let forall validators gym (html: Lazy<HtmlDocument>) =
    List.forall (function
        | HtmlValidator f -> f (html.Force())
        | JsonValidator f -> f gym
    ) validators

// Does the gym have a tutorial in one of the accepted languages?
let hasTutorial acceptedLanguages =
    let inner (html: HtmlDocument) =
        let isTutorialAnchor (a: HtmlNode) =
            let text = a.InnerText().Trim()
            let startsWithTutorial = text.StartsWith "Tutorial"
            let endsWithLanguage = acceptedLanguages
                                   |> List.exists (fun lang -> text.EndsWith ("(" + lang + ")"))
            startsWithTutorial && endsWithLanguage

        html.Descendants ["a"]
        |> Seq.tryFind isTutorialAnchor
        |> Option.isSome

    HtmlValidator inner

let difficultyBetween low high =
    JsonValidator (fun gym -> gym.Difficulty >= low && gym.Difficulty <= high)

// Check if the contest has standard input and output
let hasStandardIO =
    let inner (html: HtmlDocument) =
        html.CssSelect "table.problems .notice"
        |> List.forall (fun node -> node.InnerText().Contains("standard input/output"))

    HtmlValidator inner