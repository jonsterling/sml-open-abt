functor OperatorInjection
  (structure Operator : PARSE_OPERATOR
   structure Universe : OPERATOR_UNIVERSE where
     type world = Operator.world) :>
  INJECTION
    where type t = Operator.t
    where type ambient = Universe.t =
struct
  type ambient = Universe.t
  exception Mismatch
  open Operator

  local
    val operations = {toString = toString, eq = eq, arity = arity, parse = parseOperator}
    val {inject : t -> ambient, project} = Universe.embed ((), operations)
  in
    val `> = inject
    fun `< t =
      case project t of
           SOME a => a
         | NONE => raise Mismatch
  end
end
