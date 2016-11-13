open Env

(* Expressions: all PL/0 expressions have integer values *)
(***********************************************************
 * eval.ml
 * Copyright 2016 by Xuanrui Qi <me@xuanruiqi.com> 
 * Licensed under Mozilla Public License. 
 * Available at
 * 
 *     https://www.mozilla.org/en-US/MPL/2.0/ 
 * 
 * Covered Software is provided under this License 
 * on an “as is” basis, without warranty of any kind, 
 * either expressed, implied, or statutory, including, 
 * without limitation, warranties that the Covered Software 
 * is free of defects, merchantable, fit for a particular 
 * purpose or non-infringing. No liability is assumed unless 
 * required by law or consented by writing. Refer to 
 * license for details. *)
open Env

type arithop = Plus | Minus | Mult | Div

type exp = Literal  of int
         | Var      of ident
         | Const    of ident
         | Arith    of arithop * exp * exp

(* Conditions: conditions are PL/0 booleans *)
type relop = Eq | Neq | Lt | Leq | Gt | Geq

type cond = Rel of relop * exp * exp
          | Odd of exp

(* Statements: statments are not side-effect free, as PL/0 is imperative *)
type statement = Input    of ident
               | Output   of exp
               | DefConst of ident * exp
               | DefVar   of ident
               | Assign   of ident * exp
               | Block    of statement list
               | Call     of ident
               | Ifx      of cond * statement * statement
               | Whilex   of cond * statement

exception NotImplemented of string

let rec evalExp (exp, rho, xi) =
match exp with
| Literal v -> v
| Var     x -> lookup (x, rho)
| Const   c -> lookup (c, xi)
| Arith (op, e1, e2) -> 
  match op with
  | Plus  -> evalExp (e1, rho, xi) + evalExp (e2, rho, xi) 
  | Minus -> evalExp (e1, rho, xi) - evalExp (e2, rho, xi)
  | Mult  -> evalExp (e1, rho, xi) * evalExp (e2, rho, xi)
  | Div   -> evalExp (e1, rho, xi) / evalExp (e2, rho, xi)

let rec evalCond (condition, rho, xi) = 
match condition with
| Rel (op, e1, e2) -> (
  match op with
  | Eq  -> evalExp (e1, rho, xi) =  evalExp (e2, rho, xi) 
  | Neq -> evalExp (e1, rho, xi) != evalExp (e2, rho, xi) 
  | Lt  -> evalExp (e1, rho, xi) <  evalExp (e2, rho, xi) 
  | Leq -> evalExp (e1, rho, xi) <= evalExp (e2, rho, xi) 
  | Gt  -> evalExp (e1, rho, xi) >  evalExp (e2, rho, xi)
  | Geq -> evalExp (e1, rho, xi) >= evalExp (e2, rho, xi))
| Odd e -> let isOdd n = if (n mod 2) = 0 then true else false
           in isOdd (evalExp (e, rho, xi))

let rec evalStmt (stmt, rho, xi) = 
match stmt with
| Input  x           -> raise (NotImplemented "INPUT is not implemented")
| Output e           -> raise (NotImplemented "OUTPUT is not implemented")
| 
| Assign (x, e)      -> (rho, bind (x, evalExp (e, rho, xi), xi))
| Block  block       -> (
  match block with
  | s :: ss -> let (rho', xi') = evalStmt (s, rho, xi)
               in  evalStmt (Block ss, rho', xi')
  | []      -> (rho, xi))     
| Call   f           -> raise (NotImplemented "CALL is not implemented")
| Ifx    (c, st, sf) -> if   evalCond (c, rho, xi) 
                        then evalStmt (st, rho, xi)
                        else evalStmt (sf, rho, xi)
| Whilex (c, st)   -> if evalCond (c, rho, xi)
                      then let (rho', xi') = evalStmt (st, rho, xi)
                           in  evalStmt (Whilex (c, st), rho', xi')
                      else (rho, xi)