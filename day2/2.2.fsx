let getSteps levels =
  levels
  |> Seq.pairwise
  |> Seq.map (fun (fst, snd) -> snd - fst)

let isSafeChange down step =
  if step = 0 then false
  elif abs step >= 1 && abs step <= 3 then true
  elif down && step < 0 then true
  elif not down && step > 0 then true
  else false

let areSafeSteps steps =
  let isSafeChangeTest = isSafeChange (Seq.head steps < 0)
  not (steps |> Seq.exists (fun step -> not (isSafeChangeTest step)))
  && not ((steps |> Seq.exists (fun step -> step > 0)) && (steps |> Seq.exists (fun step -> step < 0)))


System.IO.File.ReadLines("./2.1.input")
|> Seq.map (fun line -> line.Split " " |> Array.map int)
|> Seq.map getSteps
|> Seq.filter areSafeSteps
|> Seq.length
|> printfn "%A"
