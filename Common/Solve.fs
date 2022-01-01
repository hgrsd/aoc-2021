module Common.Solve

let fromFile parser =
    System.IO.File.ReadLines
    >> List.ofSeq
    >> List.map parser

let run solutions parsed =
    solutions
    |> List.iter (fun solution -> solution parsed)

let solve parser solutions = fromFile parser >> run solutions
