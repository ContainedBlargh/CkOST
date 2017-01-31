// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open Trans

let fromFile input = 
  printfn "%s" input
  let program = Parse.fromFile input
  printfn "Program was parsed to\n%A" program
  program

let INSTRUCTIONS = "This programs takes two parameters, -i <input file> and optionally -o <output file> \n"

let toFile program path = 
  File.WriteAllText(path, (transpile program))

[<EntryPoint>]
let main argv = 
  let rec parseArgs arglist (acc : (string option) * (string option)) = 
    match arglist with
    | [] -> acc
    | "-i"::xs -> parseArgs (List.tail xs) (Some ((List.head xs)), snd acc)
    | "-o"::xs -> parseArgs (List.tail xs) (fst acc, Some (List.head xs))
    | _::xs -> (printfn "%s" INSTRUCTIONS);parseArgs xs acc
  let arglist = Array.toList argv
  let parameters = (parseArgs arglist (None,None))
  match parameters with
  | (Some input, Some output) -> toFile (fromFile input) output
  | (Some input, None) -> toFile (fromFile input) "out"
  | (_,_) -> (printfn "%s" INSTRUCTIONS)
  0 // return an integer exit code
