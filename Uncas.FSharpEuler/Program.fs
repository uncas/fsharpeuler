type Number(value) =
    member this.Value = value
    member this.IsMultipleOf divisor =
        value % divisor = 0
    member this.IsMultipleOfEither x y =
        this.IsMultipleOf x || this.IsMultipleOf y
    member this.ValueIfEven =
        if this.IsMultipleOf 2 then value else 0
    member this.ValueIfMultipleOfEither x y =
        if this.IsMultipleOfEither x y then value
        else 0

[<AbstractClass>]
type NumberInSequence<'T>(value) =
    inherit Number(value)
    abstract member Next : 'T

type Linear(value, step) =
    inherit NumberInSequence<Linear>(value)
    override this.Next = Linear(value + step, step)

type Fibonacci(value, next) =
    inherit NumberInSequence<Fibonacci>(value)
    override this.Next = Fibonacci(next, value + next)
    static member First = Fibonacci(1, 2)

type IInclusionStrategy =
    abstract member IncludeValue: Number -> bool

type FizzBuzzStrategy() =
    interface IInclusionStrategy with
        member __.IncludeValue(number) =
            number.IsMultipleOfEither 3 5
            
let rec addNext (number:Linear) (inclusionStrategy:IInclusionStrategy) max =
    if number.Value > max then 0
    else (if inclusionStrategy.IncludeValue(number) then number.Value else 0 ) +
            addNext number.Next inclusionStrategy max

let euler1 =
    addNext (Linear(0, 1)) (FizzBuzzStrategy()) 999

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
