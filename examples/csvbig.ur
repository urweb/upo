table t : { A : int, B : int, C : int, D : int, E : int, F : int, G : int, H : int, I : int, J : int, K : int, L : int }

fun main () : transaction page =
    Csv.importTable t "";
    return <xml></xml>
