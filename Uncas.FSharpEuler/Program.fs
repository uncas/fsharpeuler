type private Fibonacci(value, next) =
    member this.Value = value
    member this.Next() =
        Fibonacci(next, value + next)
    static member First = Fibonacci(1, 2)

let isMultipleOf number divisor =
    number % divisor = 0

let isMultipleOf3Or5 number =
    isMultipleOf number 3 || isMultipleOf number 5

let numberIfMultipleOf3Or5 number =
    if isMultipleOf3Or5 number then number
    else 0

let rec addNext number max =
    if number >= max then 0
    else numberIfMultipleOf3Or5 number + addNext (number+1) max

let euler1 =
    addNext 0 1000

let numberIfEven number =
    if isMultipleOf number 2 then number
    else 0

let rec addNextEvenFibonacci fib1 fib2 max =
    if fib2 > max then 0
    else numberIfEven fib2 + addNextEvenFibonacci fib2 (fib1+fib2) max

let euler2 =
    addNextEvenFibonacci 1 2 4000000

let euler actual expected =
    printfn "(%A) %A should be %A" (actual = expected) actual expected

[<EntryPoint>]
let main argv = 
    euler euler1 233168
    euler euler2 4613732
    printfn "(%A)" (Fibonacci.First.Next().Value)
    0 // return an integer exit code
