datatype value = 
    NumVal of real
  | BoolVal of bool
  | StringVal of string
  | Closure of string list * expr * env
  | PrimOp of (value list -> value)

and expr = (* and is used to define a new type from an existing type, nutally recursive *)
    NumExpr of real
  | VarExpr of string
  | StringExpr of string
  | IfExpr of expr * expr * expr
  | LetExpr of (string * expr) list * expr
  | AnonExpr of string list * expr
  | AppExpr of expr * expr list

withtype env = (string * value) list (*  use withtype to  define a new type fron an existing type *)

(* define some functions using pattern matching, slightly different than racket *)

fun serialize (NumVal n) = Real.toString n
  | serialize (BoolVal true) = "true"
  | serialize (BoolVal false) = "false"
  | serialize (StringVal s) = "\"" ^ s ^ "\""
  | serialize (Closure _) = "#<procedure>"
  | serialize (PrimOp _) = "#<primop>"

fun equalityFn [NumVal a, NumVal b] = BoolVal (Real.== (a, b))
  | equalityFn [BoolVal a, BoolVal b] = BoolVal (a = b)
  | equalityFn [StringVal a, StringVal b] = BoolVal (a = b)
  | equalityFn _ = BoolVal false

val initialEnv = [
    ("+", PrimOp (fn [NumVal a, NumVal b] => NumVal (a + b) | _ => raise Fail "OAZO: Invalid arguments for +")),
    ("-", PrimOp (fn [NumVal a, NumVal b] => NumVal (a - b) | _ => raise Fail "OAZO: Invalid arguments for -")),
    ("*", PrimOp (fn [NumVal a, NumVal b] => NumVal (a * b) | _ => raise Fail "OAZO: Invalid arguments for *")),
    ("/", PrimOp (fn [NumVal a, NumVal b] => if Real.!= (b, 0.0) then NumVal (a / b) else raise Fail "OAZO: Division by zero"
                   | _ => raise Fail "OAZO: Invalid arguments for /")),
    ("<=", PrimOp (fn [NumVal a, NumVal b] => BoolVal (a <= b) | _ => raise Fail "OAZO: Invalid arguments for <=")),
    ("equal?", PrimOp equalityFn),
    ("true", BoolVal true),
    ("false", BoolVal false),
    ("error", PrimOp (fn [v] => raise Fail ("user-error: " ^ serialize v) | _ => raise Fail "OAZO: Invalid arguments for error"))
]

open ListPair;

fun interp (NumExpr n) env = NumVal n
  | interp (StringExpr s) env = StringVal s
  | interp (IfExpr (cond, thenExpr, elseExpr)) env =
    (case interp cond env of
         BoolVal true => interp thenExpr env
       | BoolVal false => interp elseExpr env
       | _ => raise Fail "OAZO: Invalid condition for if")
  | interp (AnonExpr (params, body)) env = Closure (params, body, env)
  | interp (AppExpr (fnExpr, argExprs)) env =
    case interp fnExpr env of
       Closure (params, body, closureEnv) =>
       let
         val args = map (fn e => interp e env) argExprs
         (* Use ListPair.zip to pair each parameter with its corresponding argument value *)
         val paramArgPairs = 
            if length(params) = length(args) then
              ListPair.zip (params, args)
            else
              raise Fail "OAZO: Parameter and argument lists have unequal lengths"
         (* Extend the closureEnv with these new bindings *)
         val newEnv = paramArgPairs @ closureEnv
       in
         interp body newEnv
       end
       (* | PrimOp op => op (map (fn e => interp e env) argExprs) *)
       | _ => raise Fail "OAZO: Invalid function application"

(* Write some simple test cases for interp *)
fun testInterp () =
    let val result = interp (NumExpr 42.0) initialEnv
    in print (serialize result) end

fun testInterp2 () =
    let val result = interp (AppExpr (VarExpr "+", [NumExpr 40.0, NumExpr 2.0])) initialEnv
    in print (serialize result) end

(* fun testInterp3 () =
    let val result = interp (AppExpr (VarExpr "error", [NumExpr 42.0])) initialEnv
    in serialize result
    handle Fail msg => "OAZO: " ^ msg *)

fun testInterp4 () =
    let val result = interp (LetExpr ([("x", NumExpr 42.0)], VarExpr "x")) initialEnv
    in serialize result end


fun topInterp expr =
    let val result = interp expr initialEnv
    in serialize result end
    handle Fail msg => "OAZO: " ^ msg
