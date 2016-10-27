namespace ProjectEuler

//open Microsoft.FSharp.Math

module Answers =

    let problem1 = 
        
        let rec numbers current max = 
            match current with
            | c when c >= max -> []
            | _ -> 
                let next = current + 1
                if (current % 5) = 0 || current % 3 = 0 then
                    current :: (numbers next max)
                else
                    numbers next max

        let matching = numbers 0 1000

        printfn "Sum is %d" (List.sum matching)


    let problem2 = 
        
        let rec fib x x' max =
            match x with
            | i when i >= max -> []
            | _ -> 
                let current = x + x'
                x :: fib x' current max

        let values = fib 1 2 4000000
        let even = values |> List.where (fun x -> x % 2 = 0)

        printfn "%d" (List.sum even)


    let problem3 =

        let factorLimit x = int64 (ceil (sqrt (float x)))

        let rec factors (current:int64) (x:int64) (limit:int64) = 
            match current with
            | c when c >= limit -> []
            | c when x % c = 0L -> c :: factors (current + 1L) x limit
            | _ -> factors (current + 1L) x limit
        
        let test = factors 2L 927729L (927729L/2L)

        let rec mapFactors numbers x =
            let n = List.head numbers
            match n with 
            | c when c > x / 2L -> []
            | c when x % c = 0L -> n :: mapFactors numbers.Tail x
            | _ -> mapFactors numbers.Tail x

        let rec isPrime' (x:int64) (div:int64) =
            match div with
            | c when c > x / 2L -> true
            | c when x % c = 0L -> false
            | _ -> isPrime' x (div + 1L)

        let isPrime x = isPrime' x 2L

        let rec primesErato current numbers = 
            if List.isEmpty numbers then []
            else
                let filtered = numbers |> List.where (fun x -> x % current <> 0L)
                if List.isEmpty filtered then []
                else current :: primesErato filtered.Head filtered
        
        //let x = 13195L
        let x = 600851475143L
        let limit = factorLimit x
        printfn "%A" ((factors 2L x limit) |> List.where isPrime)


    let problem4 = 
        
        let x = 9009
        
        let rec reverseLastDigits n x = 
            match n with
            | c when c <= 0 -> 0
            | _ -> 
                let remainder = if x = 0 then 0 else x % 10
                let integer = x / 10
                let current = remainder * (pown 10 (n-1))
                let next = reverseLastDigits (n-1) integer 
                //printfn "rem:%A int:%A n:%A" remainder integer n
                current + next

        let countDigits x = 
            let rec countDigits x n =
                let shift = x / 10
                //printfn "%A" shift
                if shift > 0 then countDigits shift n+1
                else 0
            (countDigits x 0) + 1

        let firstDigits n x = x / (pown 10 n)

        let isPalindrome x = 
            let digits = countDigits x 
            let half' = digits / 2
            let half = if digits % 2 = 0 then half' else half'-1
            let first = firstDigits half' x
            let last = reverseLastDigits half x
            //printfn "%A : %A = " first last
            first = last
        
        let getMultiplesTo x =
            let rec findUp limit x start =
                //printfn "%A x %A = %A" x start (x*start)
                if x > limit then [] 
                else (start, x, (start * x)) :: (findUp limit (x+1) start)

            let rec findBiggest limit x =
                match x with
                | 0 -> []
                | _ -> let mutliples = findUp limit x x
                       match mutliples with
                       | [] -> []
                       | _ -> mutliples :: findBiggest limit (x-1)

            findBiggest x x

        let third (_, _, x) = x
        let multiples = getMultiplesTo 999 |> List.concat |> List.where (fun x -> isPalindrome (third x))
        printfn "Others: %A" multiples

        let findBiggestPalindrome x lowerBound = 
            let rec findUp limit x start =
                //printfn "%A x %A = %A" x start (x*start)
                if x > limit then []
                else if isPalindrome (start * x) then (start, x, start * x) :: findUp limit (x+1) start
                else findUp limit (x+1) start

            let rec findBiggest limit x =
                match x with
                | x when x = lowerBound -> []
                | _ -> findUp limit x x :: findBiggest limit (x-1)

            List.concat (findBiggest x x)

        let startN = 861168 + 1
        let endN = 998001
        let rec findPalindrome x y = 
            if x = y then []
            else
                if isPalindrome x then x :: findPalindrome (x+1) y
                else findPalindrome (x+1) y

        //let other = findPalindrome startN endN
        //printfn "%A" other
        let biggest = (findBiggestPalindrome 999 900) |> List.maxBy (fun (_, _, x) -> x)
        printfn "%A" (biggest)

    let problem5 = 
    
        let divisors = [2..20]
        
        let isBiggestComposite list n = 
            list |> Seq.where (fun x -> n < x && x % n = 0) |> Seq.isEmpty

        let divisors' = divisors |> Seq.where (fun x -> isBiggestComposite divisors x) |> List.ofSeq |> List.rev

        let start = List.last divisors';
        let startTime = System.DateTime.Now
        let blah = 
            Seq.initInfinite (fun x -> x * start) 
            |> Seq.skip start
            |> Seq.where (fun x -> divisors' |> Seq.forall (fun y -> x % y = 0))
            |> Seq.take 1
            |> List.ofSeq
        let endTime = System.DateTime.Now.Subtract startTime;
        endTime.TotalMilliseconds

    let problem8 =
        
        let numberString = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

        let numbers = numberString |> Seq.map (fun x -> x.ToString() |> System.Int64.Parse) |> List.ofSeq
        
        let blockSize = 13

        let multiplyBlock block = block |> Seq.fold (*) 1L

        let getBlock i x = 
            match i with
            | c when c >= numbers.Length - blockSize -> (i, -1L)
            | _ -> (i, numbers |> Seq.skip i |> Seq.take blockSize |> multiplyBlock)

        let maxIndex = numbers 
                        |> Seq.mapi getBlock
                        |> Seq.maxBy snd

        numbers |> Seq.skip (fst maxIndex) |> Seq.take blockSize |> List.ofSeq
        
