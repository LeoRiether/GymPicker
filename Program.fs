open FSharp.Data
open FSharp.Data.JsonExtensions

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

let loadGymPage id =
    Gym.urlFromId id
    |> HtmlDocument.Load


[<EntryPoint>]
let main argv =

    let validate = Validate.forall [
        Validate.difficultyBetween 4 4;
        Validate.hasTutorial ["en"; "pt"];
        Validate.standardIO;
    ]

    printfn "Loading gyms..."
    let gyms =
        Gym.loadAll ()
        |> Gym.takeMostRecent 512
        |> randomShuffle

    let printProgressMessage (gym: Gym.T) =
        printfn "Testing %d (%s)..." gym.Id.Value gym.Name

    let chosen =
        gyms
        |> Seq.find (fun gym ->
            printProgressMessage gym
            let html = lazy (loadGymPage gym.Id)
            validate gym html
        )

    printfn ""
    printfn "Found suitable contest!"
    printfn "%A" chosen
    printfn ""
    printfn "%s" (Gym.urlFromId chosen.Id)

    0
