module Common.Util

open System

module Bit =
  let valueAt i number =
    let mask = 1 <<< (11 - i)
    if (number &&& mask) <> 0 then 1 else 0

  let fromBool: bool -> int = Convert.ToInt32

  let listToInt bits =
    bits
    |> List.map fromBool
    |> List.fold (fun (i, acc) cur -> (i, (acc ||| cur) <<< if i = bits.Length then 0 else 1)) (0, 0)
    |> snd
