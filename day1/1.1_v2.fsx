let list1, list2 =
  System.IO.File.ReadLines("./1.1.input")
  |> Seq.map (
    fun line ->
    let values = line.Split " "
    int values[0], int values[3])
  |> List.ofSeq
  |> List.unzip

let sortedList1 = List.sort list1
let sortedList2 = List.sort list1

List.zip sortedList1 sortedList2
|> List.map (fun (a, b) -> (a - b))
//|> List.sum
|> System.Console.WriteLine
