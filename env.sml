(*Note that 'a Env is short for environment*)

(*
 *[EXCEPTION] NameNotBound of string
 * An exception that is raised when a string is not not bound to a value in the
 * given 'a Env. 
 *)
exception NameNotBound of string;

(*
 *[TYPE] 'a Env = string -> 'a
 * A closure that maps a string to the polymorphic type 'a.
 * This is a function type that, when given a string, returns the value
 * associated with that string. Note that 'a allows for any value type to be stored in the environment.
 *)
type 'a Env = string -> 'a;

(*
 *[FUNCTION] env_new = fn : unit -> 'a Env
 * Returns a new 'a Env.
 * Note that new 'a Envs don't have any mappings, and as such any lookup 
 * of a string results in a "NameNotBound" exception.
 * Argument must be the unit value.
 *)
fun env_new () : 'a Env = fn x => raise NameNotBound "NameNotBound";

(*
 *[FUNCTION] env_lookup = fn : 'a Env -> string -> 'a
 * Returns the value associated with the given name in the given 'a Env. A
 * NameNotBound is thrown if the name is not bound in the 'a Env.
 * Arguments must be an 'a Env and a string.
 *)
fun env_lookup (env:'a Env) name = env name;

(*
 *[FUNCTION] env_bind = fn : 'a Env -> string -> 'a -> 'a Env
 * Adds the given name and value mapping into the given 'a Env. Note that
 * such dynamic building is only possible by creating a new 'a Env.
 * Arguments must be an 'a Env, a string, and some value with type 'a.
 *)
fun env_bind (env:'a Env) name value : 'a Env = 
    fn lookup => if name = lookup
		 then value
		 else env lookup
;
