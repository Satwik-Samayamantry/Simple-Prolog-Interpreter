exception NotFound


type const = string
type var = string
type argument = Variable of var | Number of int | Constant of const * (argument list)
type environment = (var * argument) list
type clause = Clause of const * (argument list)
type head = Head of clause
type body = Body of (clause list)
type predicate = Fact of head | Rule of head * body
type database = predicate list
type query = Query of (clause list)


let rec existsInList x y = match y with
    [] -> false
  | h::t -> (x = h) || (existsInList x t)
;;

let rec listUnion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if (existsInList h l2) then listUnion t l2
            else ((h)::(listUnion t l2))
;;

let rec print_argument (a:argument) = 
    match a with
    | Variable(v) -> Printf.printf " %s " v
    | Number(n) -> Printf.printf " %d " n
    | Constant(s,l) -> Printf.printf " %s " s
;;

let rec printSolution (env:environment) = 
    match env with 
         [] -> Printf.printf "true"
         | [(v,t)] -> (
             Printf.printf "%s =" v;
             print_argument t;
         )
         | (v,t)::xs -> (
            Printf.printf "%s =" v;
            print_argument t;
            Printf.printf ", ";
            printSolution xs;
         )
;;

let varsInClause (Clause(c,l): clause) : var list = 
    match (List.hd l) with
    | Variable(v) -> [v]
    | _ -> [] 
;;

let varsInQuery (query: query) : var list =
    match query with
    Query(q) -> varsInClause (List.hd q)
;;

let get1char () =
    let argumentio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { argumentio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN argumentio;
    res

let rec getSolution (env:environment) (vars:var list) =
    match vars with
    | [] -> []
    | (v::vs) ->
        let rec occurs l =
            match l with
             [] -> raise NotFound
            | (x::xs) -> if (fst x) = v then x else occurs xs in
            try
                occurs (env)::getSolution env vs
            with NotFound -> getSolution env vs
;;
