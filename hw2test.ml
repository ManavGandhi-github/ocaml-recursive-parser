(* Define the types for nonterminals *)
type bool_expr_nonterminals =
  | Expr | AndExpr | NotExpr | Bool

(* Define the grammar for simple boolean expressions *)
let bool_expr_grammar =
  (Expr,
   function
     | Expr ->
         [[N AndExpr];
          [N AndExpr; T "||"; N Expr]]
     | AndExpr ->
         [[N NotExpr];
          [N NotExpr; T "&&"; N AndExpr]]
     | NotExpr ->
         [[T "!"; N NotExpr];
          [N Bool]]
     | Bool ->
         [[T "true"]; [T "false"]])

(* Accept all function for testing *)
let accept_all _ = Some []

(* Acceptor function to test for empty suffix *)
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

(* Test case for make_matcher with the boolean expression grammar *)
let make_matcher_test =
  (make_matcher bool_expr_grammar accept_all ["true"; "&&"; "true"; "||"; "false"] = Some [])


(* Test case for make_parser with the boolean expression grammar and checking parse_tree_leaves *)
let make_parser_test =
  let frag = ["true"; "&&"; "true"; "||"; "false"] in
  match make_parser bool_expr_grammar frag with
  | Some tree -> parse_tree_leaves tree = frag
  | _ -> false

(* Printing the results of the tests to verify *)
let () =
  print_endline ("make_matcher_test (boolean expressions): " ^ string_of_bool make_matcher_test);
  print_endline ("make_parser_test (boolean expressions): " ^ string_of_bool make_parser_test)
