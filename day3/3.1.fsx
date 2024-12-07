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


let rec getMulStartIndexes (text : string) indexes from =
  let searchStringStart = "mul("
  let searchStringEnd = ")"
  let substringIndex = text.Substring(from).IndexOf(searchStringStart)
  let index = from + substringIndex
  let nextFrom =
    if index + searchStringStart.Length < text.Length then index + searchStringStart.Length
    else searchStringStart.Length - 1
  let indexEnd = nextFrom + text.Substring(nextFrom).IndexOf(searchStringEnd)
  if  substringIndex = -1 || nextFrom = 0 || indexEnd = -1 then indexes
  else getMulStartIndexes text (indexes @ [text.Substring(index + searchStringStart.Length, indexEnd - (index + searchStringStart.Length))]) nextFrom

getMulStartIndexes text [] 0
|> Seq.filter checkString
|> Seq.map (fun x -> x.Split(','))
|> Seq.map (fun x -> int x[0] * int x[1])
|> Seq.sum
|> printfn "%A"
