type t a = a
val monad_t = mkMonad {Return = fn [a] (x : a) => x,
                       Bind = fn [a] [b] (a : t a) (b : a -> t b) => b a}
fun run [a] (m : t a) = m
