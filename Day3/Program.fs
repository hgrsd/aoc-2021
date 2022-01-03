open System
open Common.Util
open Common.Solve

let valueAt = Bit.valueAt 12

let toInt bits =
  bits
  |> List.fold
       (fun (i, acc) cur ->
         if i = bits.Length - 1 then
           (i, acc ||| cur)
         else
           (i + 1, (acc ||| cur) <<< 1))
       (0, 0)
  |> snd

let gamma values =
  values
  |> List.fold
       (fun counters bits ->
         counters
         |> List.mapi (fun i value -> value + valueAt i bits))
       [ for _ in 0 .. 11 -> 0 ]
  |> List.map
       (fun x ->
         Convert.ToInt32(
           if values.Length % 2 = 0 then
             x >= values.Length / 2
           else
             x > values.Length / 2
         ))
  |> toInt

let epsilon = gamma >> (^^^) 0b111111111111

let part1 parsed =
  printfn $"part1: {epsilon parsed * gamma parsed}"

let rec distill matcher pos inputs =
  match inputs with
  | [ x ] -> x
  | xs ->
    let expected = valueAt pos (matcher xs)

    distill
      matcher
      (pos + 1)
      (xs
       |> List.filter (fun value -> valueAt pos value = expected))

let part2 parsed =
  (distill gamma 0 parsed)
  * (distill epsilon 0 parsed)
  |> printfn "part2: %i"

let parse string = Convert.ToInt32(string, 2)

[<EntryPoint>]
let main _ =
  $"{__SOURCE_DIRECTORY__}/input"
  |> solve parse [ part1; part2 ]

  0
