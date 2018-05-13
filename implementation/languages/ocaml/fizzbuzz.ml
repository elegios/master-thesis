let rec do_between f low high = match low <= high with
  | true -> f low; do_between f (low+1) high
  | false -> ()

let rec remainder n div = match n <= 0 with
  | true -> (match n = 0 with | true -> 0 | false -> n + div)
  | false -> remainder (n-div) div

let fizzbuzz n = match remainder n 3 :: remainder n 5 :: [] with
  | [0; 0] -> print_endline "fizzbuzz"
  | [0; _] -> print_endline "fizz"
  | [_; 0] -> print_endline "buzz"
  | _ -> p_int n

let fizzbuzz_alt upTo =
  let rec recur n till3 till5 = match upTo <= n with
    | true -> ()
    | false -> (match (till3 = 0) :: (till5 = 0) :: [] with
      | [true; true] -> print_endline "fizzbuzz"; recur (n+1) 3 5
      | [true; _] -> print_endline "fizz"; recur (n+1) 3 (till5-1)
      | [_; true] -> print_endline "buzz"; recur (n+1) (till3-1) 5
      | _ -> p_int n; recur (n+1) (till3-1) (till5-1))
  in recur 1 2 4

let rec foo n = match n with
  | 0 -> ()
  | _ -> print_endline "foo"; p_int n; bar (n-1)
and bar n = match n with
  | 0 -> ()
  | _ -> print_endline "bar"; p_int n; foo (n-1)

let main =
  do_between fizzbuzz 1 100;
  fizzbuzz_alt 101;
  foo 10
