structure OperatorOperations : OPERATIONS =
struct
  type 'a t =
    {toString : 'a -> string,
     eq : 'a * 'a -> bool,
     arity : 'a -> Arity.t}

  exception Unregistered

  fun default () =
    {toString = fn _ => raise Unregistered,
     eq = fn _ => raise Unregistered,
     arity = fn _ => raise Unregistered}

  fun enrich {inject, project} (fallback : 'b t, {toString, eq, arity} : 'a t) =
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
            | NONE => #arity fallback theta)}
end

