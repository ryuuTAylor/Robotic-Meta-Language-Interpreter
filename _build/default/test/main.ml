open Rml
open OUnit2

let make_test (name : string) (expr_str : string) (val_str : string) =
  name >:: fun _ ->
  assert_equal val_str
    (Main.interp_expr Checker.Context.empty Eval.initial_env expr_str
     |> function
     | Ok x | Error (ParseError x) | Error (TypeError x) -> x)
    ~printer:(fun x -> x)

let tests =
  [
    make_test "unit" {|()|} {|()|};
    make_test "Bool: true" "true" "true";
    make_test "Bool: false" "false" "false";
    make_test "Int: zero" "0" "0";
    make_test "Int: negative" "-1" "-1";
    make_test "Int: positive" "5" "5";
    make_test "Int: forty-two" {| 42 |} {|42|};
    make_test "string" {| "zardoz" |} {|"zardoz"|};
    make_test "List 1" "1 :: []" "<list>";
    make_test "List 2" "[ \"haha\" ]" "<list>";
    make_test "List 3" "false :: [ true ]" "<list>";
    make_test "pair of ints" "(1,4)" "(1, 4)";
    make_test "self" "self" "<handle>";
    make_test "int identity function" "fun (x : int) -> x" "<function>";
    make_test "rec function def"
      "let rec f (x : int) : int = if x > 0 then x + f(x-1) else 0 in \
       f 4"
      "10";
    make_test "application" "(fun (x : int) -> x + 5) 37" "42";
    make_test "if test" "if 3 > 2 then 3 else 2" "3";
    make_test "let test" "let (x: int) = 3 in x + 2" "5";
    make_test "let test bind second pair"
      "let ((a, a) : int * int) = (2, 3) in a" "3";
    make_test "let function test"
      "(let (x: int) = 3 in (fun (y: int) -> y + x)) 5" "8";
    make_test "bop Add" "5 +5 +5 " "15";
    make_test "bop Subtract" "5 +3 -2 " "6";
    make_test "bop Multiplication" "5 * 7 " "35";
    make_test "bop Division" "16 / 4" "4";
    make_test "bop Mod" "42 % 37" "5";
    make_test "bop And" "true && false" "false";
    make_test "bop Or" "true || false" "true";
    make_test "bop Lt" "3 < 5" "true";
    make_test "bop Le" "5 <= 5" "true";
    make_test "bop Gt" "3 > 5" "false";
    make_test "bop Ge" "5 >= 5" "true";
    make_test "bop Eq 1" "3 = 5" "false";
    make_test "bop Eq 2" "\"Ha\" = \"Ha\"" "true";
    make_test "bop Ne 1" "3 <> 5" "true";
    make_test "bop Ne 2" "\"Ha\" <> \"Ha\"" "false";
    make_test "bop Cat" "\"Ha\" ^ \"Ha\"" {|"HaHa"|};
    make_test "bop pipe" "5 |> (fun (x: int) -> x + 37)" "42";
    make_test "bop cons" "5 :: 3 :: 2 :: 1 :: []" "<list>";
    make_test "bop assign" "let (x: int ref) = ref 3 in x := 2; !x" "2";
    make_test "uop negate" "- (5 + 5)" "-10";
    make_test "uop not" "not true" "false";
    make_test "uop ref" "ref 3" "<ref>";
    make_test "uop deref" "let (a: int ref) = ref 3 in !a" "3";
    make_test "basic sequence" "(); 5" "5";
    make_test "match bool true"
      "match 5 = 5 with | true -> 42 | false -> 0 end" "42";
  ]

let suite = "suite" >::: tests
let () = run_test_tt_main suite
