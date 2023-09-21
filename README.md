# handin4-PAD

dotnet fsi -r FsLexYacc.Runtime.dll Fun.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs

## PLC

### 4.1

### 4.2

### 4.3

For simplicity, the current implementation of the functional language
requires all functions to take exactly one argument. This seriously limits the programs
that can be written in the language (at least it limits what that can be written without
excessive cleverness and complications).
76 4 A First-Order Functional Language
Modify the language to allow functions to take one or more arguments. Start by
modifying the abstract syntax in Absyn.fs to permit a list of parameter names in
Letfun and a list of argument expressions in Call.
Then modify the eval interpreter in file Fun.fs to work for the new abstract
syntax. You must modify the closure representation to accommodate a list of para-
meters. Also, modify the Letfun and Call clauses of the interpreter. You will
need a way to zip together a list of variable names and a list of variable values, to get
an environment in the form of an association list; so function List.zip might be
useful.

```fsharp
type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string * expr list * expr    //<NEW>
  | Call of expr * expr list

  type value = 
  | Int of int
  | Closure of string * string list * expr * value env  //<NEW>

  let rec eval (e : expr) (env : value env) : int =
    match e with 
    | CstI i -> i
    | CstB b -> if b then 1 else 0
    | Var x  ->
      match lookup env x with
      | Int i -> i 
      | _     -> failwith "eval Var"
    | Prim(ope, e1, e2) -> 
      let i1 = eval e1 env
      let i2 = eval e2 env
      match ope with
      | "*" -> i1 * i2
      | "+" -> i1 + i2
      | "-" -> i1 - i2
      | "=" -> if i1 = i2 then 1 else 0
      | "<" -> if i1 < i2 then 1 else 0
      | _   -> failwith ("unknown primitive " + ope)
    | Let(x, eRhs, letBody) -> 
      let xVal = Int(eval eRhs env)
      let bodyEnv = (x, xVal) :: env
      eval letBody bodyEnv
    | If(e1, e2, e3) -> 
      let b = eval e1 env
      if b<>0 then eval e2 env
      else eval e3 env
    | Letfun(f, x, fBody, letBody) -> 
      let bodyEnv = (f, Closure(f, x, fBody, env)) :: env 
      eval letBody bodyEnv
    | Call(Var f, eArgs) -> 
      let fClosure = lookup env f
      match fClosure with
      | Closure (f, x, fBody, fDeclEnv) ->
        let xVals = List.fold (fun acc elem -> Int(eval elem env) :: acc) [] eArgs //<NEW>
        let argValList = List.zip x xVals       //<NEW>
        let fBodyEnv = List.fold (fun acc elem -> (elem) :: acc) ((f, fClosure) :: fDeclEnv) argValList //<NEW>
        eval fBody fBodyEnv
      | _ -> failwith "eval Call: not a function"
    | Call _ -> failwith "eval Call: not first-order function"
```

### 4.4

```
AtExpr:
    Const                               { $1                     }
  | NAME                                { Var $1                 }
  | LET NAME EQ Expr IN Expr END        { Let($2, $4, $6)        }
  | LET NAME Names EQ Expr IN Expr END   { Letfun($2, $3, $5, $7) } <NEW>
  | LPAR Expr RPAR                      { $2                     }
;

  AtExprs:
    AtExpr                              { [$1] }    <NEW>
  | AtExpr AtExprs                      { $1 :: $2} <NEW>
;

  AppExpr:
    AtExpr AtExprs                       { Call($1, $2)           } <NEW>
  | AppExpr AtExprs                      { Call($1, $2)           } <NEW>
;
```

### 4.5
