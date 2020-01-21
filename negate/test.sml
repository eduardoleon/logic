val x = Var "x"
val y = Var "y"
val z = Var "z"

val p = x /\ ~y
val q = y \/ ~z
val r = p \/ q

fun test () =
  let
  in
    print (Pretty.pretty r);
    print (Pretty.pretty (Negate.negate r))
  end
