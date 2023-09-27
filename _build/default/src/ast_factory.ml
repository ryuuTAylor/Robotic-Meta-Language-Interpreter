open Ast

let make_let_defn p e = LetDefn (p, e)
let make_let_rec_defn p e = LetrecDefn (p, e)
let make_unit_pattern () = PUnit ()
let make_wild_pattern () = PWild
let make_bool_pattern b = PBool b
let make_int_pattern n = PInt n
let make_string_pattern s = PString s
let make_var_pattern x = PVar x
let make_pair_pattern p1 p2 = PPair (p1, p2)
let make_nil_pattern () = PNil
let make_cons_pattern p1 p2 = PCons (p1, p2)
let make_unit () = Unit ()
let make_bool b = Bool b
let make_pair e1 e2 = Pair (e1, e2)
let make_int n = Int n
let make_string s = String s
let make_self () = Self
let make_var x = Var x
let make_fun p e = Fun (p, e)
let make_app e1 e2 = App (e1, e2)
let make_let p e1 e2 = Let (p, e1, e2)
let make_let_rec p e1 e2 = Letrec (p, e1, e2)
let make_nil () = Nil
let make_bop b e1 e2 = Binop (b, e1, e2)
let make_uop u e = Unop (u, e)
let make_seq e1 e2 = Seq (e1, e2)
let make_ifelse e1 e2 e3 = If (e1, e2, e3)
let make_match e cs = Match (e, cs)
let make_await p e1 e2 = Await (p, e1, e2)
let make_spawn e1 e2 = Spawn (e1, e2)
let make_send e1 e2 = Send (e1, e2)
let make_recv e = Recv e
let make_return e = Return e
