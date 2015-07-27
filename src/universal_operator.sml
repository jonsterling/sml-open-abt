structure OperatorUniverse = EnrichOpenEnded
  (structure OpenEnded = OpenEnded
   structure Operations = OperatorOperations)

structure UniversalOperator : PARSE_OPERATOR =
struct
  type t = OperatorUniverse.t
  type world = unit

  val operations = #2 o OperatorUniverse.operations

  fun eq (theta, theta') = #eq (operations ()) (theta, theta')
  fun arity theta = #arity (operations ()) theta
  fun toString theta = #toString (operations ()) theta
  fun parseOperator () = #parse (operations ())
end

