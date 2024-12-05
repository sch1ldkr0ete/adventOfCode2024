let mutable x1 = []
let mutable x2 = []
for line in System.IO.File.ReadLines("./1.1.input") do
  let values = line.Split()
  x1 <- values[0] :: x1
  x2 <- values[3] :: x2
x1 <- List.sort x1
x2 <- List.sort x2
let mutable totalDifference = 0
for i in [0 .. List.length x1-1] do
  totalDifference <- totalDifference + abs ((int x1[i]) - (int x2[i]))
System.Console.WriteLine totalDifference
