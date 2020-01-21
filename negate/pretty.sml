signature PRETTY =
sig
  val pretty : prop -> string
end

structure Pretty : PRETTY =
struct
  datatype item = Parent of prop | Child of prop | Symbol of string
  
  (*  Pretty-print the proposition.  *)
  fun break (Parent (p /\ q) :: ts, ss) = break (Child q :: Symbol " /\\ " :: Child p :: ts, ss)
    | break (Parent (p \/ q) :: ts, ss) = break (Child q :: Symbol " \\/ " :: Child p :: ts, ss)
    | break (Parent p :: ts, ss) = break (Child p :: ts, ss)
    | break (Child (Var x) :: ts, ss) = break (ts, x :: ss)
    | break (Child (~p) :: ts, ss) = break (Child p :: Symbol "~" :: ts, ss)
    | break (Child p :: ts, ss) = break (Symbol ")" :: Parent p :: Symbol "(" :: ts, ss)
    | break (Symbol s :: ts, ss) = break (ts, s :: ss)
    | break (nil, ss) = concat ss
  
  fun pretty p = break (Parent p :: nil, "\n" :: nil)
end
