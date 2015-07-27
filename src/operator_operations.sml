structure OperatorOperations : OPERATIONS =
struct
  type 'a t =
    {toString : 'a -> string,
     eq : 'a * 'a -> bool,
     arity : 'a -> Arity.t,
     parse : 'a CharParser.charParser}

  exception Unregistered

  fun default () =
    {toString = fn _ => raise Unregistered,
     eq = fn _ => raise Unregistered,
     arity = fn _ => raise Unregistered,
     parse = ParserCombinators.fail "unregistered"}

  fun enrich project (fallback : 'b t, operation : 'a t) =
    {toString =
       (fn theta =>
         case project theta of
              SOME a => #toString operation a
            | NONE => #toString fallback theta),
     eq =
       (fn (theta, theta') =>
         case (project theta, project theta') of
              (SOME a, SOME a') => #eq operation (a, a')
            | (NONE, NONE) => #eq fallback (theta, theta')
            | _ => false),
     arity =
       (fn theta =>
         case project theta of
              SOME a => #arity operation a
            | NONE => #arity fallback theta),
     parse = #parse fallback }
end

