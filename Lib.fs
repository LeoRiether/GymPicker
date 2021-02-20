module GymPicker

open FSharp.Data

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

let pick log firstN validate =
    // Load gyms through the Codeforces API
    log <| "Loading gyms..."
    let gyms =
        Gym.loadAll ()
        |> Gym.takeMostRecent firstN
        |> randomShuffle

    let logProgress (gym: Gym.T) =
        log <| sprintf "Testing %d (%s)..." gym.Id.Value gym.Name

    // Find a contest that passes the validation function
    let chosen =
        gyms
        |> Seq.tryFind (fun gym ->
            logProgress gym
            let contest = lazy (loadContest gym.Id)
            let standings = lazy (loadStandings gym.Id)
            validate gym contest standings
        )

    match chosen with
    | Some gym ->
        log <| ""
        log <| "Found suitable contest!"
        log <| sprintf "%A" gym
        log <| ""
        log <| sprintf "%s" (Gym.contestUrlFromId gym.Id)
    | None ->
        log <| ""
        log <| "Oh no! No contest was found for your criteria!"

    chosen