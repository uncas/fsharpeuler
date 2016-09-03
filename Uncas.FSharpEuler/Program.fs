type Number(value) =
    member this.Value = value
    member this.ValueIfEven =
        if value % 2 = 0 then value else 0
    member this.IsMultipleOf divisor =
        value % divisor = 0
    member this.IsMultipleOf3Or5 =
        this.IsMultipleOf 3 || this.IsMultipleOf 5
    member this.NumberIfMultipleOf3Or5 =
        if this.IsMultipleOf3Or5 then value
        else 0

type Linear(value, step) =
    inherit Number(value)
    member this.Next =
        Linear(value + step, step)

type Fibonacci(value, next) =
    inherit Number(value)
    member this.Next =
        Fibonacci(next, value + next)
    static member First = Fibonacci(1, 2)

let rec addNext (number:Linear) max =
    if number.Value >= max then 0
    else number.NumberIfMultipleOf3Or5 + addNext number.Next max

let euler1 =
    addNext (Linear(0, 1)) 1000

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
