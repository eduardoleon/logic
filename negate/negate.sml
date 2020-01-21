signature NEGATE =
sig
  val negate : prop -> prop
end

structure Negate : NEGATE =
struct
  datatype item = And | Or | Neg | Prop of prop
  
  (*  Break compound propositions into their constituent parts.  *)
  fun break (p /\ q, qs, ss) = break (p, q :: qs, And :: ss)
    | break (p \/ q, qs, ss) = break (p, q :: qs, Or :: ss)
    | break (~p, qs, ss) = break (p, qs, Neg :: ss)
    | break (p, qs, ss) = build (~p, qs, ss)
  
  (*  Build the De Morgan dual of the original proposition.  *)
  and build (q, qs, Prop p :: And :: ss) = build (p \/ q, qs, ss)
    | build (q, qs, Prop p :: Or :: ss) = build (p /\ q, qs, ss)
    | build (p, qs, Neg :: ss) = build (~p, qs, ss)
    | build (p, q :: qs, ss) = break (q, qs, Prop p :: ss)
    | build (p, _, _) = p
  
  fun negate p = break (p, nil, nil)
end
