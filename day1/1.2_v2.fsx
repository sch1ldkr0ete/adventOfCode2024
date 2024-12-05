let list1, list2 =
  System.IO.File.ReadLines("./1.1.input")
  |> Seq.map (
    fun line ->
    let values = line.Split()
    int values[0], int values[3])
  |> List.ofSeq
  |> List.unzip

let freq a =
  list1
  |> List.filter (fun x -> a = x)
  |> List.length

list2
|> List.reduce (fun acc value -> acc + value * freq value)
|> System.Console.WriteLine