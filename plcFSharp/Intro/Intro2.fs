(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr;;

type axepr = 
  |CstI of int
  |Var of string
  |Add of axepr * axepr
  |Mul of axepr * axepr
  |Sub of axepr * axepr;; 
  
//v - (W + z)
let a1 = Sub(Var "v", Add(Var "w", Var "z"));;

//2 * (v - (w + z))
let a2 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")));;


let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

(* function for 1.2.3 PLC-book *)
let rec fmt aexpr = 
    match aexpr with 
    | CstI i -> i.ToString()
    | Var x -> x
    | Add(a1, a2) -> " (" + fmt a1 + " + " + fmt a2 + ")"
    | Mul(a1, a2) -> " (" + fmt a1 + " * " + fmt a2 + ")" 
    | Sub(a1, a2) -> " (" + fmt a1 + " - " + fmt a2 + ")";; 

(* function for 1.2.4 PLC-book *)
let rec simplify aexpr = 
    match aexpr with 
    | CstI i -> CstI i
    | Var x -> Var x 
    | Add(a1, a2) -> 
       let s1 = simplify a1
       let s2 = simplify a2
       match (s1,s2) with 
       | CstI 0, _ -> s2
       | _, CstI 0 -> s1
       | _ -> Add(s1, s2)
    | Mul(a1, a2) ->
        let s1 = simplify a1
        let s2 = simplify a2
        match (s1,s2) with
        | CstI 1, _ -> s2
        | _, CstI 1 -> s1
        | CstI 0, _ -> CstI 0
        | _, CstI 0 -> CstI 0
        | _ -> Mul(s1, s2)
    | Sub(a1, a2) ->
        let s1 = simplify a1
        let s2 = simplify a2
        if s1 = s2 then CstI 0
        else
          match (s1,s2) with
          | _, CstI 0 -> s1
          | _ -> Sub(s1, s2);;
    
(* function for 1.2.5 PLC-book *)
let rec diff a v = 
    match a with
    | CstI i -> CstI 0
    | Var x -> if i = v then CstI 1 else CstI 0 
    | Add(a1, a2) -> Add(diff a1 v, diff a2 v)
    | Mul(a1, a2) -> Add(Mul(diff a1 v, a2), Mul(a1, diff a2 v))
    | Sub(a1, a2) -> Sub(diff a1 v, diff a2 v);; 

(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) -> if eval e1 env > eval e2 env then eval e1 env else eval e2 env
    | Prim("min", e1, e2) -> if eval e1 env < eval e2 env then eval e1 env else eval e2 env 
    | Prim(ope, e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env 
        match ope with 
        | "==" -> if i1 = i2 then 1 else 0
        | _    -> failwith "unknown primitive"
    | If(e1, e2, e3) -> if eval e1 env < 0 then eval e2 env else eval e3 env
    | Prim _            -> failwith "unknown primitive";;


let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let e4v = eval (Prim("==", CstI 10, CstI 5)) env;; 
let e5v = eval (Prim("max", CstI 10, CstI 5)) env;;
let e6v = eval (Prim("min", CstI 10, CstI 5)) env;;



