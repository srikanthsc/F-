// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"
let list1 = [ 1; 2; 3 ]

// Properties
printfn "list1.IsEmpty is %b" (list1.IsEmpty)
printfn "list1.Length is %d" (list1.Length)
printfn "list1.Head is %d" (list1.Head)
printfn "list1.Tail.Head is %d" (list1.Tail.Head)
printfn "list1.Tail.Tail.Head is %d" (list1.Tail.Tail.Head)
printfn "list1.Item(1) is %d" (list1.Item(1))

let sum list =
   let rec loop list acc =
       match list with
       | head :: tail -> loop tail (acc + head)
       | [] -> acc
   loop list 0


let IsPrimeMultipleTest n x =
   x = n || x % n <> 0

let rec RemoveAllMultiples listn listx =
   match listn with
   | head :: tail -> RemoveAllMultiples tail (List.filter (IsPrimeMultipleTest head) listx)
   | [] -> listx


let GetPrimesUpTo n =
    let max = int (sqrt (float n))
    RemoveAllMultiples [ 2 .. max ] [ 1 .. n ]

printfn "Primes Up To %d:\n %A" 100 (GetPrimesUpTo 100)
let continents = ["Antarctica"; "Asia"; "Africa"]
printfn "%A" continents