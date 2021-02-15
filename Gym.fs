module Gym

open FSharp.Data
open FSharp.Data.JsonExtensions

type Id =
    | Id of int
    member this.Value =
        let (Id id) = this
        id

let urlFromId (Id id) = sprintf "https://codeforces.com/gym/%d" id

type T =
    {
        Id: Id;
        Name: string;
        Difficulty: int;
    }

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
    }

let loadAll () =
    let gyms =
        Http.RequestString "https://codeforces.com/api/contest.list?gym=true"
        |> JsonValue.Parse

    gyms?result.AsArray()
    |> Array.map fromJson

let takeMostRecent n = Array.rev >> Array.take n