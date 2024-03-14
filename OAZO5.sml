datatype value =
    NumVal of real
  | BoolVal of bool
  | StringVal of string
  | Closure of string list * expr * env
  | PrimOp of (value list -> value)

and expr =
    NumExpr of real
  | VarExpr of string
  | StringExpr of string
  | IfExpr of expr * expr * expr
  | LetExpr of (string * expr) list * expr
  | AnonExpr of string list * expr
  | AppExpr of expr * expr list

withtype env = (string * value) list

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

fun interp (NumExpr n) env = NumVal n

fun topInterp expr =
    let val result = interp expr initialEnv
    in serialize result end
    handle Fail msg => "OAZO: " ^ msg