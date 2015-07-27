signature OPERATOR_UNIVERSE =
sig
  type world
  include OPEN_ENDED
    where type 'a Operations.t = unit * (world, 'a) OperatorOperationsType.t
end

functor OperatorUniverse (type world) : OPERATOR_UNIVERSE =
struct
  type world = world

  structure U = EnrichOpenEnded
    (structure OpenEnded = OpenEnded
     structure Operations = OperatorOperations (type world = world))

  open U
end

functor UniversalOperator (type world) :
sig
  include PARSE_OPERATOR
  structure Universe : OPERATOR_UNIVERSE
end =
struct
  structure Universe = OperatorUniverse (type world = world)
  type t = Universe.t
  type world = world

  val operations = #2 o Universe.operations

  fun eq (theta, theta') = #eq (operations ()) (theta, theta')
  fun arity theta = #arity (operations ()) theta
  fun toString theta = #toString (operations ()) theta
  fun parseOperator w = #parse (operations ()) w
end

