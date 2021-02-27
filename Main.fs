open Validate.Stored

// Change this to change your contest selection criteria
let validators =
    let languages = Some ["en"; "pt"]
    let tutorial = hasTutorial languages
    let discussion = hasDiscussion languages

    [
        exclude [Gym.Id 102114];
        difficultyBetween 3 4;
        either tutorial discussion;
        hasStandardIO;
        // atLeastNFromTheCountries 3 ["br"];
        usersHaveNotParticipated [];
    ]

// We only pick from the N most recent contests
[<Literal>]
let N = 512

// ------------------------------------------------------------------------------

[<EntryPoint>]
let main argv =
    let validate = forall validators

    let log = printfn "%s"
    let pick = GymPicker.pick log

    pick N validate |> ignore

    0