signature EVALUATE =
sig
  val evaluate : prop * (string * bool) list -> bool option
end

structure Evaluate : EVALUATE =
struct
  datatype item = And | Or | Neg | Value of bool
  
  fun lookup (x, (y, t) :: ts) = if x = y then SOME t else lookup (x, ts)
    | lookup _ = NONE
  
  fun break (p /\ q, qs, ss, ts) = break (p, q :: qs, And :: ss, ts)
    | break (p \/ q, qs, ss, ts) = break (p, q :: qs, Or :: ss, ts)
    | break (~p, qs, ss, ts) = break (p, qs, Neg :: ss, ts)
    | break (Var x, qs, ss, ts) =
      case lookup (x, ts) of
          SOME p => build (p, qs, ss, ts)
        | NONE => NONE
  
  and build (q, qs, Value p :: And :: ss, ts) = build (p andalso q, qs, ss, ts)
    | build (q, qs, Value p :: Or :: ss, ts) = build (p orelse q, qs, ss, ts)
    | build (p, qs, Neg :: ss, ts) = build (not p, qs, ss, ts)
    | build (p, q :: qs, ss, ts) = break (q, qs, Value p :: ss, ts)
    | build (p, _, _, _) = SOME p
  
  fun evaluate (p, ts) = break (p, nil, nil, ts)
end
