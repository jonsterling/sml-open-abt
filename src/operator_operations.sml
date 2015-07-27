structure OperatorOperationsType =
struct
  type ('world, 'a) t =
    {toString : 'a -> string,
     eq : 'a * 'a -> bool,
     arity : 'a -> Arity.t,
     parse : 'world -> 'a CharParser.charParser}
end

functor OperatorOperations (type world) : OPERATIONS =
struct
  type 'a t = (world, 'a) OperatorOperationsType.t

  exception Unregistered

  fun default () =
    {toString = fn _ => raise Unregistered,
     eq = fn _ => raise Unregistered,
     arity = fn _ => raise Unregistered,
     parse = fn _ => ParserCombinators.fail "unregistered"}

  open ParserCombinators
  infix 5 ||
  infix 6 wth

  fun enrich (inject, project) (fallback : 'b t, {toString, eq, arity, parse} : 'a t) =
    {toString =
       (fn theta =>
         case project theta of
              SOME a => toString a
            | NONE => #toString fallback theta),
     eq =
       (fn (theta, theta') =>
         case (project theta, project theta') of
              (SOME a, SOME a') => eq (a, a')
            | (NONE, NONE) => #eq fallback (theta, theta')
            | _ => false),
     arity =
       (fn theta =>
         case project theta of
              SOME a => arity a
            | NONE => #arity fallback theta),
     parse =
       (fn w => #parse fallback w || parse w wth inject)}
end

