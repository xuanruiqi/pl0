(***********************************************************
 * env.ml
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

type ident = string
type 'a env = (ident * 'a) list

exception UnboundIdent of string
exception Match

let emptyEnv = []

let bind (name, value, env) = (name, value) :: env

let rec bindList (names, values, env) = 
    match (names, values) with
    | ([], []) -> env
    | ((_, []) | ([], _)) -> raise Match
    | (x :: xs, v :: vs) -> bindList (xs, vs, bind (x, v, env))

let rec lookup (name, env) =
    match env with
    | []           -> raise (UnboundIdent ("Identifier " ^ name ^ " is unbound"))
    | (x, v) :: bs -> if name = x then v else lookup (name, bs)
