open System
open Common.Util
open Common.Solve

let asInt bits =
  let mutable x = 0

  bits
  |> List.iteri
       (fun i bit ->
         if i = bits.Length - 1 then
           x <- x ||| bit
         else
           x <- (x ||| bit) <<< 1)

  x

let asBits number =
  seq { 0 .. 12 }
  |> Seq.map (fun i -> Bit.getBitValue number (11 - i))
  |> List.ofSeq

let computeGamma values =
  values
  |> List.fold
       (fun counters bits ->
         counters
         |> List.mapi (fun i value -> value + Bit.getBitValue value (11 - i)))
       [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
  |> List.map (fun x -> Convert.ToInt32(x >= values.Length / 2))
  |> asInt

let part1 parsed =
  let gamma = computeGamma parsed
  let epsilon = gamma ^^^ 0b111111111111
  printfn $"{epsilon * gamma}"

let parse string = Convert.ToInt32(string, 2)

[<EntryPoint>]
let main _ =
  $"{__SOURCE_DIRECTORY__}/input"
  |> solve parse [ part1 ]

  0
