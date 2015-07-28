functor OperatorInjection (Operator : OPERATOR) :>
  INJECTION
    where type t = Operator.t
    where type ambient = OperatorUniverse.t =
struct
  type ambient = OperatorUniverse.t
  exception Mismatch
  open Operator

  local
    val operations = {toString = toString, eq = eq, arity = arity}
    val {inject : t -> ambient, project} = OperatorUniverse.embed ((), operations)
  in
    val `> = inject
    val `<? = project
    fun `< t =
      case `<? t of
           SOME a => a
         | NONE => raise Mismatch
  end
end
