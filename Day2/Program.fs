open Common

type Command =
  | Down of int
  | Up of int
  | Forward of int

let mult (x, y) = x * y
let coords (x, y, _) = (x, y)

// part1
let f1 (x, y) = function
  | Down value -> (x, y + value)
  | Up value -> (x, y - value)
  | Forward value -> (x + value, y)
  
let part1 = List.fold f1 (0, 0) >> mult >> printfn "part 1: %i"

// part 2
let f2 (x, y, aim) = function
  | Down value -> (x, y, aim + value)
  | Up value -> (x, y, aim - value)
  | Forward value -> (x + value, y + aim * value, aim)
  
let part2 = List.fold f2 (0, 0, 0) >> coords >> mult >> printfn "part 2: %i"

let parse (line: string) =
  let split = line.Split()

  match Array.get split 0, int (Array.get split 1) with
  | "down", value -> Down value
  | "up", value -> Up value
  | "forward", value -> Forward value
  | _ -> failwith "unsupported command"

[<EntryPoint>]
let main _ =
  $"{__SOURCE_DIRECTORY__}/input" |> solve parse [part1; part2]

  0
