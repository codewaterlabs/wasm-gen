type func('ret) = string
and func1('a1, 'ret) = (string, string)
and func2('a1, 'a2, 'ret) = (string, string, string)
and call1('a1, 'ret) = (func1('a1, 'ret), expr('a1))
and expr(_) =
  /* Constants */
  | Int(int): expr(int)
  | Bool(bool): expr(bool)
  | Float(float): expr(float)
  /* Locals */
  | Local(string): expr('a)
  | LInt(string): expr(int)
  | LBool(string): expr(bool)
  | LFloat(string): expr(float)
  /* Comparisons */
  | Eq(expr('a), expr('a)): expr(bool)
  | Lt(expr('a), expr('a)): expr(bool)
  | Gt(expr('a), expr('a)): expr(bool)
  /* Arithmetics */
  | Mul(expr('a), expr('a)): expr('a)
  | Add(expr('a), expr('a)): expr('a)
  | Sub(expr('a), expr('a)): expr('a)
  /* Control flow */
  | Call(func('ret)): expr('ret)
  | Call1(expr('a1)): expr('ret)
  | Call2(func2('a1, 'a2, 'ret), expr('a1), expr('a2)): expr('ret)
  | If(expr(bool), expr('a), expr('a)): expr('a)
  | Placeholder: expr('a);

/* Dsl operators */
let (+::) = (arg1: expr('a), arg2: expr('a)) => Add(arg1, arg2);

let (-::) = (arg1: expr('a), arg2: expr('a)) => Sub(arg1, arg2);

let ( *:: ) = (arg1: expr('a), arg2: expr('a)) => Mul(arg1, arg2);

let (==::) = (arg1: expr('a), arg2: expr('a)) => Eq(arg1, arg2);

let (>::) = (arg1: expr('a), arg2: expr('a)) => Gt(arg1, arg2);

let (<::) = (arg1: expr('a), arg2: expr('a)) => Lt(arg1, arg2);

/* Function with 1 argument */
module F1 = {
  /* DeclSpec to be passed to Decl functor */
  module type DeclSpec = {let ident: string; type a1; type ret;};
  /* DeclInst to be returned from Decl functor */
  module type DeclInst = {
    include DeclSpec;
    let a1: expr(a1);
    let call: expr(a1) => expr(ret);
  };
  /* Decl functor is first pass for function definition, and
     includes some helpful code for FuncSpec, variables
     for arguments, and recursive call helper */
  module Decl =
         (Spec: DeclSpec)
         : (DeclInst with type a1 = Spec.a1 and type ret = Spec.ret) => {
    include Spec;
    let a1: expr(Spec.a1) = Local("a1");
    let call = (a1: expr(Spec.a1)) : expr(Spec.ret) => Call1(a1);
  };
  /* Funcspec to be passed to make, includes impl */
  module type FuncSpec = {include DeclSpec; let impl: expr(ret);};
  /* Function instance, meant to be usable in dsl usage */
  module type Inst = {include FuncSpec; let getImpl: unit => expr(ret);};
  /* Make function instance */
  module Make =
         (Spec: FuncSpec)
         : (Inst with type a1 := Spec.a1 and type ret := Spec.ret) => {
    let ident = Spec.ident;
    let impl = Spec.impl;
    let getImpl = () => impl;
  };
};