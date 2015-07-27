functor OperatorInjection (Operator : PARSE_OPERATOR where type world = unit) :>
  INJECTION
    where type t = Operator.t
    where type ambient = UniversalOperator.t =
struct
  type ambient = UniversalOperator.t
  exception Mismatch
  open Operator

  local
    val operations = {toString = toString, eq = eq, arity = arity, parse = parseOperator ()}
    val (inject : t -> ambient, project) = OperatorUniverse.embed ((), operations)
  in
    val `> = inject
    fun `< t =
      case project t of
           SOME a => a
         | NONE => raise Mismatch
  end
end
