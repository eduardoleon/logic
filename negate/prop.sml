infix 6 /\ \/

datatype prop
  = Var of string
  | /\ of prop * prop
  | \/ of prop * prop
  | ~ of prop
