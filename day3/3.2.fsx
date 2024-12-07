let text =
  System.IO.File.ReadLines("./3.1.input")
  |> Seq.reduce (fun acc line -> acc + line)

let checkString testString =
  let allowedDigits = [0..999] |> List.map (fun x -> string(x)) |> Set.ofList
  if
    String.filter (fun x -> x = ',') testString
    |> String.length > 1 then false
  else
    let values = testString.Split(",")
    Set.contains values[0] allowedDigits && Set.contains values[1] allowedDigits


let rec getMulStartIndexes (fullText : string) relevantStrings from =
  let subText = fullText.Substring(from)
  let dontIndex = subText.IndexOf("don't()")
  let doIndex =
    if dontIndex <> -1 then subText.Substring(dontIndex).IndexOf("do()")
    else -1
  let searchStringStart = "mul("
  let searchStringEnd = ")"
  let startMatchIndex = subText.IndexOf(searchStringStart)
  let endMatchIndex =
    if startMatchIndex <> - 1 then subText.Substring(startMatchIndex).IndexOf(searchStringEnd)
    else -1
  if (dontIndex <> -1 && doIndex = -1 && startMatchIndex > dontIndex) || startMatchIndex = -1 || endMatchIndex = -1 then relevantStrings
  elif dontIndex <> -1 && startMatchIndex > dontIndex then getMulStartIndexes fullText relevantStrings (from + dontIndex + doIndex)
  else getMulStartIndexes fullText (relevantStrings @ [subText.Substring(startMatchIndex + searchStringStart.Length, endMatchIndex - searchStringStart.Length)]) (from + startMatchIndex + searchStringStart.Length)

getMulStartIndexes text [] 0
|> Seq.filter checkString
|> Seq.map (fun x -> x.Split(','))
|> Seq.map (fun x -> int x[0] * int x[1])
|> Seq.sum
|> printfn "%A"
