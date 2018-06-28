use "env.sml";
Control.Print.printDepth := 32;

exception ExprNotInt of string;
exception ExprNotBool of string;
exception CannotApply of string;

(*
 * [TYPE] Expr =  Bool of bool
 *	       |Int of int
 *	       |Add of Expr * Expr
 *	       |Cond of Expr * Expr * Expr
 *	       |Id of string
 *	       |Let of ((string*Expr) list) * Expr
 *	       |Def of string * Expr
 *	       |Fun of Expr * Expr
 *	       |App of Expr * Expr
 *	       |Seq of Expr list
 *	       |Disp of Expr
 *	       |Nothing;
 * Representation of the set of expressions that make up the language:
 *      > Closure of Expr * Expr * (Expr Env): closure representation     
 *	> Bool of bool: boolean representation
 *	> Int of int: integer representation
 *	> Add of Expr * Expr: addition representations
 *	> Cond of Expr * Expr * Expr: conditional representation
 *	> Id of string: identifier representation
 *	> Let of ((string*Expr) list) * Expr: let representation
 *	> Def of string * Expr: name definition representation
 *	> Fun of Expr * Expr: anonymous function representation
 *	> App of Expr * Expr: function call/ application representation
 *	> Seq of Expr list: sequence of expressions representation
 *	> Disp of Expr: display operator representation
 *	> Nothing: unit constructor/ null representation
 *)
datatype Expr = Closure of Expr * Expr * (Expr Env)
	       |Bool of bool
	       |Int of int
	       |Add of Expr * Expr
	       |Cond of Expr * Expr * Expr
	       |Id of string
	       |Let of ((string*Expr) list) * Expr
	       |Def of string * Expr
	       |Fun of Expr * Expr
	       |App of Expr * Expr
	       |Seq of Expr list
	       |Disp of Expr
	       |Nothing
;
  
(*
 *[FUNCTION] e2s = fn : 'a Env -> string
 * Returns a string representation of the given 'a Env.
 * Argument must be an 'a Env.
 *)
  fun e2s (Closure (param,body,env)) = "fn* " ^ e2s(param) ^ " => " ^ e2s(body)
    | e2s (Bool b) = (Bool.toString b) ^ ":bool"
    | e2s (Int i) = (Int.toString i) ^ ":int"
    | e2s (Add (x,y)) =  e2s(x) ^ " + " ^ e2s(y)
    | e2s (Cond (cond,conseq,alt)) =
      "IF(" ^ e2s(cond) ^ ")THEN{" ^ e2s(conseq) ^ "}ELSE{" ^ e2s(alt) ^ "}"
    | e2s (Id n) = n ^ ":id"
    | e2s (Let (tempBinds, body)) =
      "LET{" ^ e2sLetHelper(tempBinds) ^ "}IN{" ^ e2s(body) ^ "}END"
    | e2s (Def (name,value)) = "val " ^ name ^ " = " ^ e2s(value)
    | e2s (Fun (param, body)) = "fn " ^ e2s(param) ^ " => " ^ e2s(body)
    | e2s (App (func, arg)) = e2s(func) ^ " " ^ e2s(arg)
    | e2s (Seq (exprSeq)) = "{\n" ^ e2sSeqHelper(exprSeq) ^ "}"
    | e2s (Disp (expr)) = "print(" ^ e2s(expr) ^ ")"
    | e2s (Nothing) = "null"
  and e2sLetHelper nil = ""
    | e2sLetHelper ((name, value)::nil) =  e2s (Def (name, value))
    | e2sLetHelper ((name, value)::t) = e2s (Def (name, value)) ^ ","
					^ e2sLetHelper t
  and e2sSeqHelper nil = ""
    | e2sSeqHelper (expr::nil) = e2s expr ^ "\n"
    | e2sSeqHelper (expr::t) = e2s expr ^ ",\n"
			       ^ e2sSeqHelper t
;

(*
 *[FUNCTION] eval = fn : Expr Env -> Expr -> Expr Env * Expr
 * Evaluates the given Expr in respect to the given Expr Env,
 * and returns a Expr Env * Expr tuple
 * Arguments must be an Expr Env and an Expr
 *     > Closure constants evaluate to themselves
 *     > Bool constants evaluate to themselves
 *     > Int constants evaluate to themselves
 *     > Nothing constants evaluate to themselves
 *     > Add takes two Ints and returns a Int as the sum; raises an exception 
 *       if the arguments are not Ints
 *     > Cond evaluates the Bool (as the first Expr), and evaluates the second
 *       Expr if the condition is true, or evaluates the third Expr if false;
 *       raises an exception if the first argument is not a Bool
 *     > Let takes a list of bindings, represented as string * Expr tuples, 
 *       binds them into a temporary environment, and then evaluates the second
 *       Expr with respect to that temporary binding; the original environment
 *       is returned
 *     > Def binds the given string to the given Expr into the given Expr Env;
 *       the returned Env has the new binding, and the Expr returned is Def 
 *     > Id looks up the given string with respect to the given Expr Env, and 
 *       returns the (first) value bound to that string, or raises an exception
 *       if no such binding exists
 *     > Fun creates a 1-parameter anonymous function with the first Expr as 
 *       the parameter, and the second Expr as the body
 *     > App performs a function call, where the first Expr is the abstraction,
 *       and the second Expr is the application
 *     > Seq evaluates the Expr in the Expr list, and the last evaluation of 
 *       the list is the return Expr of the evaluation
 *     > Disp prints the string of the given Expr; returns Disp as the Expr
 *)
  fun eval env (c as Closure(x,y,z)) = (env, c)
    | eval env (b as Bool x) = (env, b)
    | eval env (i as Int x) = (env, i)
    | eval env Nothing = (env, Nothing)
    | eval env (Add(expr1, expr2)) = (evalAdd env expr1 expr2)
    | eval env (Cond(cond,consq, alt))= (evalCond env cond consq alt)
    | eval env (Let(tempBinds, body)) =
      (env, #2(eval (addTempBindings env tempBinds) body))
    | eval env (d as Def(name, expr)) =
      ((env_bind env name (getExpr env expr)), d)
    | eval env (Id n) = (env, (env_lookup env n))
    | eval env (Fun(param, body)) = (env, Closure(param, body, env))
    | eval env (App(function, argument)) = (application env function argument)
    | eval env (Seq seqList)  = (evalSeq env seqList)
    | eval env (d as Disp expr) = ((print (e2s expr));(env, d))
    (*| eval env expr = (env, expr)*)(*Catch-all*)
  and getExpr env expr = #2(eval env expr)
  and getEnv env expr = #1(eval env expr)
  and addTempBindings env nil = env
    | addTempBindings env ((name, value)::b) =
      addTempBindings (env_bind env name value) b
  and evalAdd env (Int a1) (Int a2) = (env, Int (a1 + a2))
    | evalAdd env (Bool b) expr2 =  raise ExprNotInt "Gave Bool, need Int"
    | evalAdd env expr1 (Bool b) =  raise ExprNotInt "Gave Bool, need Int"
    | evalAdd env (Nothing) expr2 =  raise ExprNotInt "Gave Nothing, need Int"
    | evalAdd env expr1 (Nothing) =  raise ExprNotInt "Gave Nothing, need Int"
    | evalAdd env (Closure(x,y,z)) expr2 = raise ExprNotInt "Gave Closure, need Int"
    | evalAdd env expr1 (Closure(x,y,z)) = raise ExprNotInt "Gave Closure, need Int"
    | evalAdd env (Def(x,y)) expr2 =  raise ExprNotInt "Gave Def, need Int"
    | evalAdd env expr1 (Def(x,y)) =  raise ExprNotInt "Gave Def, need Int"
    | evalAdd env (Disp x) expr2 =  raise ExprNotInt "Gave Disp, need Int"
    | evalAdd env expr1 (Disp x) =  raise ExprNotInt "Gave Disp, need Int"
    | evalAdd env expr1 expr2  = (evalAdd env (getExpr env expr1) (getExpr env expr2))
  and evalCond env (Bool b) consq alt =
      if b
      then (eval env consq)
      else (eval env alt)
    | evalCond env (Int x) ex1 ex2 =  raise ExprNotBool "Gave Int, need Bool"
    | evalCond env (Nothing) ex1 ex2 =  raise ExprNotBool "Gave Nothing, need Bool"
    | evalCond env (Closure(x,y,z)) ex1 ex2 =  raise ExprNotBool "Gave Closure, need Bool"
    | evalCond env (Def(x,y)) ex1 ex2 =  raise ExprNotBool "Gave Def, need Bool"
    | evalCond env (Disp x) ex1 ex2 =  raise ExprNotBool "Gave Disp, need Bool"
    | evalCond env expr1 expr2 expr3 = (evalCond env (getExpr env expr1) expr2 expr3) 
  and evalSeq env nil = (env, Nothing)
    | evalSeq env [expr] = (eval env expr)
    | evalSeq env (expr::t) = (evalSeq (getEnv env expr) t)
  and application env (Fun((Id i), body)) arg =
      (eval env (Let([(i, arg)], body)))
    | application env expr1 expr2 = raise CannotApply "CannotApply"; 
 ;
