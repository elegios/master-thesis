let rec fibrec n = match n with
  | 1 -> 1
  | 2 -> 1
  | _ -> fibrec (n-1) + fibrec (n-2)

let fib n =
  let rec recur n a b = (match n with
    | 1 -> a
    | _ -> recur (n-1) b (a+b))
  in recur n 1 1

let rec do_between f low high = match low <= high with
  | true -> f low; do_between f (low+1) high
  | false -> ()

let main =
  do_between (fun n -> p_int (fibrec n)) 1 10;
  do_between (fun n -> p_int (fib n)) 1 10
