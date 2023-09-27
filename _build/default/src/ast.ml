(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, handle, uop, bop) are used by the parser and type-checker.
   You do not want to change them.
 ******************************************************************************)

type id = string
type handle = int

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Cat
  | Pipe
  | Cons
  | Assign
  | Bind

and uop =
  | Neg
  | Not
  | Ref
  | Deref

(******************************************************************************
   [pat] is the type of the AST for patterns. You may implement
   this type however you wish. Look at the formal semantics and think about other
   AST types we have worked with for inspiration.
 ******************************************************************************)

type pat =
  | PUnit of unit
  | PBool of bool
  | PInt of int
  | PString of string
  | PVar of string
  | PCons of pat * pat
  | PPair of pat * pat
  | PNil
  | PList of pat * pat
  | PWild

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr =
  | Unit of unit
  | Bool of bool
  | Int of int
  | String of string
  | Binop of bop * expr * expr
  | Unop of uop * expr
  | Var of string
  | If of expr * expr * expr
  | Let of pat * expr * expr
  | Letrec of pat * expr * expr
  | Fun of pat * expr
  | App of expr * expr
  | Pair of expr * expr
  | Seq of expr * expr
  | Nil
  | Self
  | List of expr * expr
  | Match of expr * (pat * expr) list
  | Await of pat * expr * expr
  | Spawn of expr * expr
  | Send of expr * expr
  | Recv of expr
  | Return of expr

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement this type
   however you wish.  There are only two kinds of definition---the let
   definition and the let [rec] definition---so this type can be quite simple.
 ******************************************************************************)
and defn =
  | LetDefn of pat * expr
  | LetrecDefn of pat * expr

(******************************************************************************
   [prog] is the type of the AST for an RML program. You should 
   not need to change it.
 ******************************************************************************)

type prog = defn list
