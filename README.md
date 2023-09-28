# handin4-PAD

dotnet fsi -r FsLexYacc.Runtime.dll Absyn.fs Fun.fs FunPar.fs FunLex.fs Parse.fs ParseAndRun.fs

## PLC

### 4.1

The following code is from our `ParseAndRun.fs`-file

```fsharp
let prog1 = "let x = 7 in let y = 13 in x + y end end";;
let prog2 = "let number = 16 in if 4 < number then number else 4 + number end";;
let prog3 = "let earth = 10 in let wind = 45 in let and fire = fire + earth * wind in and 15 end end end";;

prog1 |> fromString |> run;; // Returns 20
prog2 |> fromString |> run;; // Returns 16
prog3 |> fromString |> run;; // Returns 465
```

### 4.2

The following code is from our `ParseAndRun.fs`-file

```fsharp
let sum1000 = "let sum x = if x = 0 then 0 else x + sum (x-1) in sum 1000 end";;
let pow3to8 = "let pow3 x = if x = 0 then 1 else 3 * pow3 (x-1) in pow3 8 end"
let powwow = "let pow3 x = if x = 0 then 1 else 3 * pow3 (x-1) in let powsum x = if x = 0 then pow3 0 else pow3 x + powsum (x-1) in powsum 11 end end"
let powwowow = "let topow8 x = x * x * x * x * x * x * x * x in let powsum x = if x = 0 then 0 else topow8 x + powsum (x-1) in powsum 10 end end";;

sum1000 |> fromString |> run;; // Returns 500500
pow3to8 |> fromString |> run;; // Returns 6561
powwow |> fromString |> run;; // Returns 265720
powwowow |> fromString |> run;; // Returns 167731333
```

All functions return the expected output.

### 4.3

The Following Code is from Absyn.fs

```fsharp
type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string list * expr * expr    //<NEW>
  | Call of expr * expr list
```

The Following Code is from Fun.fs

```fsharp
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

The following code is from FunPar.fsy

```text
%start Main
%type <Absyn.expr> Main Expr AtExpr Const
%type <Absyn.expr> AppExpr
%type <string list> Names         <NEW>
%type <Absyn.expr list> AtExprs   <NEW>

AtExpr:
    Const                               { $1                     }
  | NAME                                { Var $1                 }
  | LET NAME EQ Expr IN Expr END        { Let($2, $4, $6)        }
  | LET NAME Names EQ Expr IN Expr END   { Letfun($2, $3, $5, $7) } <NEW>
  | LPAR Expr RPAR                      { $2                     }
;

Names:                                                <NEW>
    NAME                                { [$1] }      <NEW>
  | NAME Names                          { $1 :: $2 }  <NEW>
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

FunPar.fsy

```text
%token ELSE END FALSE IF IN LET NOT THEN TRUE
%token PLUS MINUS TIMES DIV MOD
%token EQ NE GT LT GE LE AND OR     //<NEW>
%token LPAR RPAR 
%token EOF

...

%left OR      //<NEW>           /* lowest precedence  */
%left AND     //<NEW>
%left ELSE              
%left EQ NE 
%left GT LT GE LE
%left PLUS MINUS
%left TIMES DIV MOD 
%nonassoc NOT                   /* highest precedence  */

...

Expr:
    AtExpr                              { $1                     }
  | AppExpr                             { $1                     }
  | IF Expr THEN Expr ELSE Expr         { If($2, $4, $6)         }
  | MINUS Expr                          { Prim("-", CstI 0, $2)  }
  | Expr PLUS  Expr                     { Prim("+",  $1, $3)     }
  | Expr MINUS Expr                     { Prim("-",  $1, $3)     }
  | Expr TIMES Expr                     { Prim("*",  $1, $3)     }
  | Expr DIV   Expr                     { Prim("/",  $1, $3)     } 
  | Expr MOD   Expr                     { Prim("%",  $1, $3)     }
  | Expr EQ    Expr                     { Prim("=",  $1, $3)     }
  | Expr NE    Expr                     { Prim("<>", $1, $3)     }
  | Expr GT    Expr                     { Prim(">",  $1, $3)     }
  | Expr LT    Expr                     { Prim("<",  $1, $3)     }
  | Expr GE    Expr                     { Prim(">=", $1, $3)     }
  | Expr LE    Expr                     { Prim("<=", $1, $3)     }
  | Expr AND   Expr                     { If($1, $2, FALSE)      }  //<NEW>
  | Expr OR    Expr                     { If($1, TRUE, $2)       }  //<NEW>
;

AppExpr:
    AtExpr AtExpr                       { Call($1, $2)           }
  | AppExpr AtExpr                      { Call($1, $2)           } //<REMOVED>
;
```

FunLex.fsl

```text
rule Token = parse
  | [' ' '\t' '\r'] { Token lexbuf }
  | '\n'            { lexbuf.EndPos <- lexbuf.EndPos.NextLine; Token lexbuf }
  | ['0'-'9']+      { CSTINT (System.Int32.Parse (lexemeAsString lexbuf)) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*
                    { keyword (lexemeAsString lexbuf) }
  | "(*"            { commentStart := lexbuf.StartPos;
                      commentDepth := 1; 
                      SkipComment lexbuf; Token lexbuf }
  | '='             { EQ }
  | "<>"            { NE }
  | '>'             { GT }
  | '<'             { LT }
  | ">="            { GE }
  | "<="            { LE }
  | "&&"            { AND }         //<NEW>
  | "||"            { OR }          //<NEW>
  | '+'             { PLUS }                     
  | '-'             { MINUS }                     
  | '*'             { TIMES }                     
  | '/'             { DIV }                     
  | '%'             { MOD }
  | '('             { LPAR }
  | ')'             { RPAR }
  | eof             { EOF }
  | _               { failwith "Lexer error: illegal symbol" }
```
