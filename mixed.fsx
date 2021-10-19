let isPrime x = 
    match x with
    | 0 | 1 -> false
    | 2 -> true
    | _ -> 

    {2..int (ceil( sqrt( float x)))}     
    |> Seq.exists (fun y -> x % y = 0)
    |> not

let rec naturalSeq start step = 
    seq { 
        yield start
        yield! naturalSeq (start + step) step 
    }

let primeSeq l =
    naturalSeq 0 1        
    |> Seq.filter (fun x -> isPrime x)
    |> Seq.take l

let primeSeq'  =
    naturalSeq 0 1        
    |> Seq.filter (fun x -> isPrime x)