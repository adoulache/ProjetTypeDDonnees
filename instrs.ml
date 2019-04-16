(* Instructions of the CAM *)

open Miniml;;

type instr =
  PrimInstr of primop
| Cons
| Push
| Swap
| Return
| Quote of value
| Cur of code
| App
| Branch of code * code
(* new for recursive calls *)
| Call of var
| AddDefs of (var * code) list
| RmDefs of int
and value =
  NullV 
| VarV of Miniml.var
| IntV of int
| BoolV of bool
| PairV of value * value
| ClosureV of code * value
and code = instr list
  
type stackelem = Val of value | Cod of code

let rec exec = function
	(* Opérations Unaires *) 
	(PairV(x,y),PrimInstr(UnOp(Fst))::c,st) -> exec(x,c,st)
|	(PairV(x,y),PrimInstr(UnOp(Snd))::c,st) -> exec(y,c,st)
	(* Opérations Arithmetiques *)	
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BArith(BAadd)))::c,st) -> exec(IntV(x+y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BArith(BAsub)))::c,st) -> exec(IntV(x-y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BArith(BAmul)))::c,st) -> exec(IntV(x*y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BArith(BAdiv)))::c,st) -> exec(IntV(x/y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BArith(BAmod)))::c,st) -> exec(IntV(x mod y),c,st)
	(* Opérations Binaires *)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BCompar(BCeq)))::c,st) -> exec(BoolV(x = y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BCompar(BCge)))::c,st) -> exec(BoolV(x >= y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BCompar(BCgt)))::c,st) -> exec(BoolV(x > y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BCompar(BCle)))::c,st) -> exec(BoolV(x <= y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BCompar(BClt)))::c,st) -> exec(BoolV(x < y),c,st)
|	(PairV(IntV(x),IntV(y)),PrimInstr(BinOp(BCompar(BCne)))::c,st) -> exec(BoolV(x <> y),c,st)

|	(t,Quote(v)::c,st) -> exec(v,c,st)
|	(x,Cons::c,Val(y)::st) -> exec(PairV(y,x),c,st)
|	(x,Push::c,st) -> exec(x,c,Val(x)::st)
|	(x,Swap::c,Val(y)::st) -> exec(y,c,Val(x)::st)
|	(x,Cur(c1)::c,st) -> exec(ClosureV(c1,x),c,st)
|	(PairV(ClosureV(cd,y),z),App::c,st) -> exec(PairV(y,z),cd,Cod(c)::st)
|	(x,Return::c,Cod(d)::st) -> exec(x,d,st)
|	(BoolV(true),Branch(t,e)::c,Val(x)::st) -> exec(x,t,Cod(c)::st)
|	(BoolV(false),Branch(t,e)::c,Val(x)::st) -> exec(x,e,Cod(c)::st)
|	config -> config;;

let rec compile env  = function
	Var(v) ->
| 	Bool ->
| 	Int ->
| 	PrimOp ->
| 	Cond ->
| 	Pair ->
| 	App ->
| 	Fn ->
| 	Fix ->

