let text =
  System.IO.File.ReadLines("./3.1.test")
  |> Seq.reduce (fun acc line -> acc + line)

let rec getMulStartIndexes (text : string) indexes from =
  let searchString = "mul("
  let index = from + text.Substring(from).IndexOf(searchString)
  printfn "%A" index
  printfn "----"
  let nextFrom =
    if index + searchString.Length < text.Length then index + searchString.Length
    else searchString.Length - 1
  if index = -1 then indexes
  else getMulStartIndexes text (index::indexes) nextFrom

getMulStartIndexes text [] 0
|> Seq.iter (fun index -> printfn "%A" index)
