fun isEmpty [K] [r ::: {K}] (fl : folder r) =
  @fold [fn _ => bool]
   (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] _ => False)
   True fl

fun isEmpty' [K] [tf :: {K} -> Type] [r ::: {K}] (fl : folder r) =
    @fold [fn r => option (tf [] -> tf r)]
     (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] _ => None)
     (Some (fn x => x)) fl
