val isEmpty : K --> r ::: {K} -> folder r -> bool
val isEmpty' : K --> tf :: ({K} -> Type) -> r ::: {K} -> folder r -> option (tf [] -> tf r)
