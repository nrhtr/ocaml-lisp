open Core.Std

type atom =
    | Integer of int
    | Symbol of string
    | Bool of bool
    | Nil

type sexpr =
    | Atom of atom
    | List of sexpr list

let print_atom = function
    | Integer n -> print_int n
    | Symbol s -> print_string s
    | Bool b -> print_string (Bool.to_string b)
    | Nil -> print_string "nil"

let rec print_expr = function
    | List l -> print_string "("; List.iteri ~f:(fun i e -> if i <> 0 then print_string " "; print_expr e) l; print_string ")"
    | Atom a -> print_atom a

let print_exprn e = print_expr e; print_string "\n"

let rec eval_list = function
    | [] -> Atom Nil
    | Atom a :: exprs -> begin match a with
        | Symbol ("+") -> int_binop (+) exprs
        | Symbol ("-") -> int_binop (-) exprs
        | Symbol ("*") -> int_binop ( * ) exprs
        | Symbol ("/") -> int_binop (/) exprs
        | Symbol ("%") -> int_binop (mod) exprs
        | Symbol ("=") -> if List.length exprs <= 1 then raise (Failure "=: Too few arguments.");
            let r = List.fold ~init:(true,(List.hd_exn exprs)) ~f:(fun acc x ->
                match acc with
                | (true, p) -> (p = x, x)
                | b -> b
            ) exprs in begin match r with
                | (true, _) -> Atom (Bool true)
                | _ -> Atom (Bool false)
            end
        | Symbol ("if") -> begin match exprs with
            | cond :: t :: f :: [] ->
                    if (eval cond = Atom (Bool true)) then
                        eval t
                    else
                        eval f
            | _ -> raise (Failure "Invalid if-form")
            end
        | _ -> raise (Failure "Invalid atom in app-form")
        end
    | List l :: exprs ->
            raise (Failure "Function lookups on non-atom expressions not yet supported");
and eval = function
    | List sexpr -> eval_list sexpr
    | x -> x
    (*
    | _ -> raise (Failure "Unsupported expression");
    *)
and int_binop op exprs =
    List.reduce_exn ~f:(fun a b ->
        match a, b with
        | Atom (Integer x), Atom (Integer y) -> Atom (Integer (op x y))
        | x, y -> begin match (eval x, eval y) with
            | Atom (Integer x'), Atom (Integer y') -> Atom (Integer (op x' y'))
            | _ -> raise (Failure "BinOp: Expected 2 integer arguments");
        end
    ) exprs

type token =
    | Open
    | Close
    | Integer of int
    | Symbol of string

let lex_new_buffer ~f:lexer ~init:n stream =
    let buffer = Buffer.create 1 in
    Buffer.add_char buffer n;
    lexer buffer stream

let lex_into_buffer ~f:lexer ~next:n ~buffer stream =
    Buffer.add_char buffer n;
    lexer buffer stream

(* TODO: symbol -> atom, create actual symbols, tokens for builtin operators?  *)
let rec lex =
    parser
        | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream
        | [< ' ('('); stream >] -> [< 'Open; lex stream >]
        | [< ' (')'); stream >] -> [< 'Close; lex stream >]
        | [< ' ('+'); stream >] -> [< 'Symbol "+"; lex stream >]
        | [< ' ('-'); stream >] -> [< 'Symbol "-"; lex stream >]
        | [< ' ('*'); stream >] -> [< 'Symbol "*"; lex stream >]
        | [< ' ('/'); stream >] -> [< 'Symbol "/"; lex stream >]
        | [< ' ('%'); stream >] -> [< 'Symbol "%"; lex stream >]
        | [< ' ('='); stream >] -> [< 'Symbol "="; lex stream >]
        | [< ' ('0' .. '9' as n); stream >] -> lex_new_buffer ~f:lex_integer ~init:n stream
        | [< ' ('a' .. 'z' as c); stream >] -> lex_new_buffer ~f:lex_symbol ~init:c stream
        | [< >] -> [< >]
and lex_integer buffer =
    parser
        | [< ' ('0' .. '9' as n); stream >] -> lex_into_buffer ~f:lex_integer ~next:n ~buffer stream
        | [< stream=lex >] ->
                [< 'Integer (Buffer.contents buffer |> Int.of_string); stream >]
and lex_symbol buffer =
    parser
        | [< ' ('a' .. 'z' as c); stream >] -> lex_into_buffer ~f:lex_symbol ~next:c ~buffer stream
        | [< stream=lex >] ->
                [< 'Symbol (Buffer.contents buffer); stream >]

let rec parse: token Stream.t -> sexpr =
    parser
        | [< 'Open; stream >] -> parse_list [] stream
        | [< 'Integer n>] -> Atom (Integer n)
        | [< 'Symbol  s>] -> Atom (Symbol s)
and parse_list acc =
    parser
        | [< 'Close; stream >] -> List (List.rev acc)
        | [< e=parse; stream >] -> parse_list (e :: acc) stream
