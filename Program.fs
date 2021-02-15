open FSharp.Data
open Validate

let languages = Some ["en"; "pt"]
let tutorial = hasTutorial languages
let discussion = hasDiscussion languages

// Change this to change your contest selection criteria
let validators = [
    difficultyBetween 4 4;
    either tutorial discussion;
    hasStandardIO;
    atLeastNFromTheCountries 2 ["br"];
    // Validate.usersHaveNotParticipated ["LeoRiether"]; // soon
    // Validate.usersHaveParticipated ["Nson"];
]

// We only pick from the N most recent contests
let N = 512

// ------------------------------------------------------------------------------

let rng = System.Random()

let randomShuffle a =
    let n = Array.length a
    for i in 1..n-1 do
        let j = rng.Next(0, i+1)

        // swap a.[i] with a.[j]
        let ai = a.[i]
        a.[i] <- a.[j]
        a.[j] <- ai

    a

let loadContest id =
    Gym.contestUrlFromId id
    |> HtmlDocument.Load

let loadStandings id =
    Gym.standingsUrlFromId id
    |> HtmlDocument.Load


[<EntryPoint>]
let main argv =

    printfn "Loading gyms..."
    let gyms =
        Gym.loadAll ()
        |> Gym.takeMostRecent N
        |> randomShuffle

    let printProgressMessage (gym: Gym.T) =
        printfn "Testing %d (%s)..." gym.Id.Value gym.Name

    let validate = forall validators

    let chosen =
        gyms
        |> Seq.tryFind (fun gym ->
            printProgressMessage gym
            let contest = lazy (loadContest gym.Id)
            let standings = lazy (loadStandings gym.Id)
            validate gym contest standings
        )

    match chosen with
    | Some gym ->
        printfn ""
        printfn "Found suitable contest!"
        printfn "%A" gym
        printfn ""
        printfn "%s" (Gym.contestUrlFromId gym.Id)
    | None ->
        printfn ""
        printfn "Oh no! No contest was found for your criteria!"

    0
