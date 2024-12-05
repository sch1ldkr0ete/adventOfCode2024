let list1, list2 =
  System.IO.File.ReadLines("./1.1.input")
  |> Seq.map (
    fun line ->
    let values = line.Split()
    int values[0], int values[3])
  |> List.ofSeq
  |> List.unzip

let mutable result = 0
for elem1 in list1 do
  let mutable counter = 0
  for elem2 in list2 do
    counter <-
      if elem1 = elem2 then counter + 1
      else counter
    result <- result + (elem1 * counter)
    counter <- 0
System.Console.WriteLine result
