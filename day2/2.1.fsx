let getSteps levels =
  levels
  |> Seq.pairwise
  |> Seq.map (fun (fst, snd) -> snd - fst)

let isSafeChange step =
  if step = 0 then false
  elif abs step >= 1 && abs step <= 3 then true
  else false

let areSafeSteps steps =
  not (steps |> Seq.exists (fun step -> not (isSafeChange step)))
  && not ((steps |> Seq.exists (fun step -> step > 0)) && (steps |> Seq.exists (fun step -> step < 0)))


System.IO.File.ReadLines("./2.1.input")
|> Seq.map (fun line -> line.Split " " |> Array.map int)
|> Seq.map getSteps
|> Seq.filter areSafeSteps
|> Seq.length
|> printfn "%A"
