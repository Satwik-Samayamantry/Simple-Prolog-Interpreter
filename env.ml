open Expression;;



exception Not_Unifiable


let rec _applyEnv (env:environment) (a:argument) : argument =
    match a with
    Constant(_) -> a
    | Number(_) -> a
    | Variable(v) -> 
        match env with
            [] -> a
            | s::xs -> if fst s = v then snd s else _applyEnv xs a
;;


let applyEnv (env: environment) (cl: clause) =
    match cl with
     Clause(c, ts) -> Clause(c, List.map (_applyEnv env) ts)
;;

let buildEnv (Clause(c1,t1):clause) (Clause(c2,t2):clause) : environment = 
    if c1 <> c2 then raise Not_Unifiable
    else match ((List.hd t1),(List.hd t2)) with
    | (Variable(v1) , Variable(v2)) -> if v1 = v2 then [] else [(v1, Variable(v2))]
    | (Variable(v1) , Constant(c)) -> [(v1,(List.hd t2))]
    | (Constant(c) , Variable(v2)) -> [(v2,(List.hd t1))]
    | (Constant(c1) , Constant(c2)) -> if c1 <> c2 then raise Not_Unifiable else []
    | _ -> raise Not_Unifiable
;;