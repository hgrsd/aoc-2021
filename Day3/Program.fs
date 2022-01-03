open System
open Common.Util
open Common.Solve

let gteHalf total n =
  if total % 2 = 0 then
    n >= total / 2
  else
    n > total / 2

let gamma values =
  values
  |> List.fold
       (fun counters bits ->
         counters
         |> List.mapi (fun i value -> value + Bit.valueAt i bits))
       [ for _ in 0 .. 11 -> 0 ]
  |> List.map (gteHalf values.Length)
  |> Bit.listToInt

let epsilon = gamma >> (^^^) 0b111111111111

let part1 parsed =
  printfn $"part1: {epsilon parsed * gamma parsed}"

let rec distill matcher pos inputs =
  match inputs, Bit.valueAt pos (matcher inputs) with
  | [ x ], _ -> x
  | xs, expected ->
    distill
      matcher
      (pos + 1)
      (xs
       |> List.filter (fun value -> Bit.valueAt pos value = expected))

let part2 parsed =
  distill gamma 0 parsed * distill epsilon 0 parsed
  |> printfn "part2: %i"

let parse string = Convert.ToInt32(string, 2)

[<EntryPoint>]
let main _ =
  $"{__SOURCE_DIRECTORY__}/input"
  |> solve parse [ part1; part2 ]

  0
