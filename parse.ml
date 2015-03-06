(*
    E1 = E1 or E2 | E2
    E2 = E2 and E3 | E3
    E3 = Variable | Number | not E3 | (E1)
*)
type expr = 
      Or of expr * expr
    | And of expr * expr
    | Not of expr
    | Variable of string
    | Number of string


let rec toString e = 
    match e with    
      Or (left, right) ->
        "(" ^ toString left ^ " | " ^ toString right ^ ")"
    | And (left, right) ->
        "(" ^ toString left ^ " & " ^ toString right ^ ")"    
    | Not (expr) ->
        "!(" ^ toString expr ^ ")"
    | Variable v -> v
    | Number v -> v

let rec toLambda e = 
    match e with
    | Or (left, right) ->          
        "((lxy.x x y) " ^ toLambda left ^ " " ^ toLambda right ^ ")"    
    | And (left, right) -> 
        "((lxy.x y (lxy.y)) " ^ toLambda left ^ " " ^ toLambda right ^ ")"
    | Not (expression) ->
        "((lx.x (lxy.y) (lxy.x)) " ^ toLambda expression  ^ ")"
    | Variable(v) -> v
    | _ -> "Error"

let isVariable l r s =
     (*print_endline ("isVar" ^ String.sub s l (r - l + 1));  
     print_int r;*)
    let rec loop i =
        if i == (r + 1)
            then true
            else
            match s.[i] with
            | '&' -> false
            | '|' -> false 
            | '!' -> false
            | _ -> loop (i + 1) in
    loop l
let  findChar c s l r = 
    let rec loop acc i = 
        if i = l - 1 
            then -1
        else if ((s.[i] = c) && (acc =0))
                then i
            else if (s.[i] ='(') 
                    then loop (acc - 1) (i - 1)
                else if (s.[i] = ')')
                         then loop (acc + 1) (i - 1)
                else loop acc (i - 1) in
    loop 0 r

let rec parseE1(*start*) l r s = 
    let pos = findChar '|' s l r in
    if (pos <> -1)
        then Or((parseE1 l (pos - 1) s), (parseE2 (pos + 1) r s)) 
    else parseE2 l r s

and  parseE2 l r s = 
    let pos = findChar '&' s l r in
    if pos <> -1 
        then And((parseE2 l (pos - 1) s), (parseE3 (pos + 1) r s)) 
    else parseE3 l r s

and parseE3 l r s = 
    if (isVariable l r s) 
        then Variable (String.sub s l (r - l + 1))
    else if (s.[l] = '!') 
            then Not(parseE3 (l + 1) r s) 
         else parseE1 (l + 1) (r - 1) s






let file = "expr.in" 
let message = "Hello!" 
  
let () =  
  (* Read file and display the first line *)
  let ic = open_in file in
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)    
    print_endline line;          (* write the result to stdout *)
    let len = String.length line in
    let my = parseE1 0 (len - 1) line in

    print_endline (toLambda my);
    flush stdout;                (* write on the underlying device now *)
    close_in ic                  (* close the input channel *) 
  
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)