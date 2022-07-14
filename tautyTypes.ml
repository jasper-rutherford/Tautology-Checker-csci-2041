(*
		Author:
			Jasper Rutherford
			
		x500:
			ruthe124
*)

type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition |
  Imply of proposition * proposition |
  Equiv of proposition * proposition |
  If of proposition * proposition * proposition ;;

let rec ifify p =
  match p with
    Not(a) -> If(ifify a, False, True) |
    And(a, b) -> If(ifify a, ifify b, False) |
    Or(a, b) -> If(ifify a, True, ifify b) |
    Imply(a, b) -> If(ifify a, ifify b, True) |
    Equiv(a, b) -> If(ifify a, ifify b, If(ifify b, False, True)) |
    True -> True |
    False -> False |
    Var a -> Var a |
    If(a, b, c) -> If(a, b, c);;

let rec normalize c =
  match c with 
    If(If(pi, a1, b1), a2, b2) -> normalize (If((normalize (pi)), (normalize (If(a1, a2, b2))), (normalize (If(b1, a2, b2))))) |
    If(pi, a1, b1) -> If(pi, (normalize (a1)), (normalize (b1))) |
    a -> a;;
    
let rec substitute c v b = 
	if c = v
	then b
	else
  	match c with  
	 	 	If(pi, a, b1) -> If((substitute pi v b), (substitute a v b), (substitute b1 v b)) |
			_ -> c;;
    
let rec simplify c = 
  match c with
    If(pi, a, b) -> 
      if pi = True
      then simplify (substitute a pi True)
      else if pi = False
      then simplify (substitute b pi False)
      else if a = True && b = False
      then pi
      else if (simplify (substitute a pi True)) = (simplify (substitute b pi False))
      then simplify (substitute a pi True)
      else If(pi, (simplify (substitute a pi True)), (simplify (substitute b pi False))) |
    _ -> c;;

let tautology p = 
  simplify (normalize (ifify p));;
						  
(*
let ifd = ifify (Imply(Not(And(Var "p", Var "q")), Or(Not(Var "p"), Not(Var "q"))));;

let norm = normalize ifd;;

let subby = (substitute (If(If((Var "x"), (Var "x"), (Var "x")), (Var "x"), (Var "x"))) (Var "x") True);;

let simp = simplify norm;; 
*)

let taut = tautology (Imply(Not(And(Var "p", Var "q")), Or(Not(Var "p"), Not(Var "q"))));;
