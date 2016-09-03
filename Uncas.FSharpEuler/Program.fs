type Number(value) =
    member this.Value = value
    member this.ValueIfEven =
        if value%2=0 then value else 0

type Fibonacci(value, next) =
    inherit Number(value)
    member this.Next =
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

let rec addNextEvenFibonacci (fib:Fibonacci) max =
    if fib.Value > max then 0
    else fib.ValueIfEven + addNextEvenFibonacci fib.Next max

let euler2 =
    addNextEvenFibonacci Fibonacci.First 4000000

let euler actual expected =
    printfn "(%A) %A should be %A" (actual = expected) actual expected

[<EntryPoint>]
let main argv = 
    euler euler1 233168
    euler euler2 4613732
    0 // return an integer exit code
