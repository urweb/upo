fun isEmpty [K] [r ::: {K}] (fl : folder r) =
  @fold [fn _ => bool]
   (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] _ => False)
   True fl
