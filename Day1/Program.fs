open Common.Solve

type State =
  { Increases: int
    Previous: int option }

let initialState = { Increases = 0; Previous = None }

let folder acc cur =
  match acc.Previous with
  | None -> { Increases = 0; Previous = Some cur }
  | Some prevVal when cur > prevVal ->
    { Increases = acc.Increases + 1
      Previous = Some cur }
  | _ ->
    { Increases = acc.Increases
      Previous = Some cur }

let sumInc = List.fold folder initialState

let part1 = sumInc >> printfn "part 1: %A"

let part2 =
  List.windowed 3
  >> List.map List.sum
  >> sumInc
  >> printfn "part 2: %A"

[<EntryPoint>]
let main _ =
  $"{__SOURCE_DIRECTORY__}/input"
  |> solve int [ part1; part2 ]

  0
