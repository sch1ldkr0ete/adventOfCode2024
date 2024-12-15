let rec getColumns (texts:seq<string>) (x, y) =
  if Seq.length texts = y || Seq.length (Seq.item y texts) = x then ""
  else string (Seq.item x (Seq.item y texts)) + getColumns texts (x, y+1)
let rec getDiagsRight (texts:seq<string>) (x, y) =
  if Seq.length texts = y || Seq.length (Seq.item y texts) = x then ""
  else string (Seq.item x (Seq.item y texts)) + getDiagsRight texts (x+1, y+1)
let rec getDiagsLeft (texts:seq<string>) (x, y) =
  if y = -1 || Seq.length (Seq.item y texts) = x then ""
  else string (Seq.item x (Seq.item y texts)) + getDiagsLeft texts (x+1, y-1)
let getAllTexts (texts:seq<string>) =
  seq {
    texts;
    (seq { 0 .. (Seq.length (Seq.item 0 texts)) } |> Seq.map (fun x -> getColumns texts (x, 0)));
    (seq { 0 .. (Seq.length (Seq.item 0 texts)) } |> Seq.map (fun x -> getDiagsRight texts (x, 0)));
    (seq { 1 .. Seq.length texts } |> Seq.map (fun y -> getDiagsRight texts (0, y)));
    (seq { 0 .. (Seq.length (Seq.item 0 texts)) } |> Seq.map (fun x -> getDiagsLeft texts (x, Seq.length texts - 1)));
    (seq { 0 .. (Seq.length texts)-2 } |> Seq.map (fun y -> getDiagsLeft texts (0, y)));
     }
  |> Seq.fold Seq.append Seq.empty
let containsXmasCount (text:string) =
  if text.Contains("XMAS") && text.Contains("SAMX") then 2
  elif text.Contains("XMAS") || text.Contains("SAMX") then 1
  else 0

System.IO.File.ReadLines("./4.1.input")
|> getAllTexts
|> Seq.map containsXmasCount
|> Seq.sum
//|> Seq.iter (fun x -> printfn "%A" x)
|> printfn "%A"
