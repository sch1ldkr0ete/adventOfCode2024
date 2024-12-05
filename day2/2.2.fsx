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
|> Seq.map (
  fun levels ->
  [0..(Array.length levels)-1]
  |> Seq.map (
    fun i ->
    (Seq.removeAt i levels)
    |> getSteps
  )
  |> Seq.exists (fun steps -> areSafeSteps steps))
|> Seq.filter (fun isSafe -> isSafe)
|> Seq.length
|> printfn "%A"
