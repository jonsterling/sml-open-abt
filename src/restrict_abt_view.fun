functor RestrictAbtView
  (structure Abt : ABT
   structure Injection : INJECTION
     where type ambient = Abt.Operator.t) :
sig
  datatype 'a view =
      $ of Injection.t * 'a vector
    | \ of Abt.Variable.t * 'a
    | ` of Abt.Variable.t

  val inject : Abt.t view -> Abt.t
  val project : Abt.t -> Abt.t view
end =
struct
  datatype 'a view =
      $ of Injection.t * 'a vector
    | \ of Abt.Variable.t * 'a
    | ` of Abt.Variable.t

  infix $
  infixr \

  structure Abt = AbtUtil(Abt)

  open Injection
  fun inject (theta $ es) = Abt.$$ (`> theta, es)
    | inject (x \ E) = Abt.\\ (x, E)
    | inject (` x) = Abt.`` x

  fun project M =
    case Abt.out M of
         Abt.$ (theta, es) => `< theta $ es
       | Abt.\ (x, E) => x \ E
       | Abt.` x => `x
end
