type State =
  { Increases: int
    Previous: int option }

let initialState = { Increases = 0; Previous = None }

let sumInc acc cur =
  match acc.Previous with
  | None -> { Increases = 0; Previous = Some cur }
  | Some prevVal when cur > prevVal ->
    { Increases = acc.Increases + 1
      Previous = Some cur }
  | _ ->
    { Increases = acc.Increases
      Previous = Some cur }

let parse = List.map int

[<EntryPoint>]
let main _ =
  let parsed =
    Common.loadFile $"{__SOURCE_DIRECTORY__}/input"
    |> parse

  // part 1
  parsed
  |> List.fold sumInc initialState
  |> printfn "%A"

  // part 2
  parsed
  |> List.windowed 3
  |> List.map List.sum
  |> List.fold sumInc initialState
  |> printfn "%A"

  0
