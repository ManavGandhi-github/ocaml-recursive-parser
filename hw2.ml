(* Conversion from Homework 1-style grammar to Homework 2-style grammar *)

(* Type definition for Homework 2-style grammar *)
type ('nonterminal, 'terminal) symbol = N of 'nonterminal | T of 'terminal

(* Function to convert grammar from Homework 1 to Homework 2 format *)
let convert_grammar (start_symbol, rules) =
  (* Function to find the expansions for a given nonterminal symbol *)
  let rec find_expansions nonterminal = function
    | [] -> []
    | (lhs, rhs)::tail ->
      if lhs = nonterminal then rhs :: find_expansions nonterminal tail
      else find_expansions nonterminal tail
  in
  let production_function nonterminal = find_expansions nonterminal rules in
  (start_symbol, production_function)

let rec parse_tree_leaves tree =
  match tree with
  | Leaf terminal -> [terminal]  (* Directly return a list containing the terminal symbol *)
  | Node (_, subtrees) -> 
      List.flatten (List.map parse_tree_leaves subtrees)
      (* Recursively process each subtree, flattening the list of lists into a single list of leaves *)


(* Complete make_matcher function *)
let make_matcher (start_symbol, production_function) accept fragment =
  (* Attempts to match a sequence of symbols against the beginning of the fragment. *)
  let rec match_sequence sequence acceptor fragment = match sequence with
    | [] -> acceptor fragment (* Empty sequence, directly invoke the acceptor *)
    | symbol :: symbols_tail ->
      match_symbol symbol (match_sequence symbols_tail acceptor) fragment
  (* Matches a single symbol (terminal or nonterminal) against the fragment. *)
  and match_symbol symbol match_continuation = match symbol with
    | T terminal -> (fun fragment -> match fragment with
                                     | head :: tail when head = terminal -> match_continuation tail
                                     | _ -> None)
    | N nonterminal -> match_nonterminal nonterminal match_continuation
  (* Tries each rule for a nonterminal in order, attempting to find a match. *)
  and match_nonterminal nonterminal match_continuation fragment =
    let alternatives = production_function nonterminal in
    try_rules alternatives match_continuation fragment
  (* Iterates over the list of rule alternatives, attempting to match each one. *)
  and try_rules rules match_continuation fragment = match rules with
    | [] -> None (* No more rules to try, fail. *)
    | first_rule :: rest_rules ->
      match match_sequence first_rule match_continuation fragment with
      | None -> try_rules rest_rules match_continuation fragment (* This rule didn't match; try the next. *)
      | Some _ as success -> success
  in
  match_nonterminal start_symbol accept fragment

(* Assuming the existence of a converted grammar and the necessary type definitions *)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let make_parser (start_symbol, production_function) =
  let rec search_paths options check_if_end chars_left =
    match options with
    | [] -> None
    | first_option::next_options -> 
      (match test_path first_option check_if_end chars_left with
        | None -> search_paths next_options check_if_end chars_left
        | Some match_found -> Some (first_option::match_found))

  and test_path path_sequence validate chars_to_test =
    match path_sequence, chars_to_test with
    | [], _ -> validate chars_to_test
    | (T term)::path_rest, first_char::chars_rest ->
        if term = first_char then 
          test_path path_rest validate chars_rest 
        else None
    | (N nonterm)::path_rest, _ -> 
        search_paths (production_function nonterm) 
          (test_path path_rest validate) chars_to_test
    | _, [] -> None

  in

  let rec build_tree from_rule trail =
    match from_rule with
    | [] -> ([], trail)
    | rule_part::rule_rest -> 
      match rule_part with
      | T leaf_val -> 
        let (built_parts, remaining_trail) = 
          build_tree rule_rest trail in
        ((Leaf leaf_val)::built_parts, remaining_trail)
      | N branch_val -> 
        let (branch_built, next_trail) = 
          match trail with
          | [] -> ([], [])
          | lead_segment::trail_rest -> build_tree lead_segment trail_rest in
        let (more_built, final_trail) = 
          build_tree rule_rest next_trail in
        ((Node (branch_val, branch_built))::more_built, final_trail)

  in

  let analyze_chars chars_to_analyze =
    match search_paths [[N start_symbol]] 
      (fun test_chars -> 
        if test_chars = [] then Some [] else None) chars_to_analyze with
    | None -> None
    | Some [] -> None
    | Some (lead_part::rest_parts) -> 
      let (tree_done, _) = 
        build_tree lead_part rest_parts in
      match tree_done with
        | [] -> None
        | top::_ -> Some top
  in
  analyze_chars
