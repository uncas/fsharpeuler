open System

let isPal str = 
    let str = str |> Seq.filter ((<>) ' ') |> Seq.toList
    str = (str |> List.rev)
    
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
    member this.IsPalindromic =
        isPal (value.ToString())


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
        member __.IncludeValue(number) = number.IsMultipleOfEither 3 5

type EvenStrategy() =
    interface IInclusionStrategy with
        member __.IncludeValue(number) = number.IsMultipleOf 2


let rec addNext (number:NumberInSequence<'T>) (strategy:IInclusionStrategy) max =
    if number.Value > max then 0
    else (if strategy.IncludeValue(number) then number.Value else 0) +
            addNext number.Next strategy max


let euler1 =
    addNext (Linear(0, 1)) (FizzBuzzStrategy()) 999

let euler2 =
    addNext (Fibonacci.First) (EvenStrategy()) 4000000


let rec largestPrime number divisor =
    if number % divisor = 0L then
        let newNumber = number/divisor
        if newNumber = 1L then divisor
        else largestPrime newNumber divisor
    else if (divisor+1L)*(divisor+1L) > number then
        number
    else
        largestPrime number (divisor+1L)

let euler3a =
    largestPrime 13195L 2L

let euler3b =
    largestPrime 600851475143L 2L


let euler4 =
    // Largest palindromic number from product of two 3-digit numbers
    printfn "%A" (Number(12123121).IsPalindromic)

let euler actual expected =
    printfn "(%A) %A should be %A" (actual = expected) actual expected


[<EntryPoint>]
let main argv = 
    euler euler1 233168
    euler euler2 4613732
    euler euler3a 29L
    euler euler3b 6857L
    euler4
    0 // return an integer exit code
