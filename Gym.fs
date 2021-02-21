module Gym

open FSharp.Data
open FSharp.Data.JsonExtensions

type Id =
    | Id of int
    member this.Value =
        let (Id id) = this
        id

let problemsUrlFromId (Id id) = sprintf "https://codeforces.com/gym/%d" id
let standingsUrlFromId id = (problemsUrlFromId id) + "/standings?showUnofficial=true"

type ProblemsHtml = ProblemsHtml of HtmlDocument
type StandingsHtml = StandingsHtml of HtmlDocument

type T =
    {
        Id: Id;
        Name: string;
        Difficulty: int;
        Problems: Lazy<ProblemsHtml>;
        Standings: Lazy<StandingsHtml>;
    }
    override this.ToString() =
        sprintf "{ Id = %d\n  Name = %s\n  Difficulty = %d }" this.Id.Value this.Name this.Difficulty

let loadProblems id =
    problemsUrlFromId id
    |> HtmlDocument.Load
    |> ProblemsHtml

let loadStandings id =
    standingsUrlFromId id
    |> HtmlDocument.Load
    |> StandingsHtml

let fromJson (value: JsonValue) =
    let id = value?id.AsInteger()
    let name = value?name.AsString()
    let difficulty = value.TryGetProperty("difficulty")
                     |> Option.map (fun x -> x.AsInteger())
                     |> Option.defaultValue 0
    {
        Id = Id id
        Name = name
        Difficulty = difficulty
        Problems = lazy (loadProblems (Id id))
        Standings = lazy (loadStandings (Id id))
    }

// Loads a Gym.T list from the Codeforces API
let loadAll () =
    let gyms =
        Http.RequestString "https://codeforces.com/api/contest.list?gym=true"
        |> JsonValue.Parse

    gyms?result.AsArray()
    |> Array.map fromJson


let takeMostRecent n = Array.rev >> Array.take n