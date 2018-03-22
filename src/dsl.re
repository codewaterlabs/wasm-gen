type expr(_) =
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
  | If(expr(bool), expr('a), expr('a)): expr('a)
  | Call(Wasm.func): expr('ret)
  | Call1(Wasm.func, expr(_)): expr('ret)
  | Call2(Wasm.func, expr(_), expr(_)): expr('ret)
  | Recurse: expr('ret)
  | Recurse1(expr(_)): expr('ret)
  | Recurse2(expr(_), expr(_)): expr('ret);

/* Dsl operators */
let (+::) = (arg1: expr('a), arg2: expr('a)) => Add(arg1, arg2);

let (-::) = (arg1: expr('a), arg2: expr('a)) => Sub(arg1, arg2);

let ( *:: ) = (arg1: expr('a), arg2: expr('a)) => Mul(arg1, arg2);

let (==::) = (arg1: expr('a), arg2: expr('a)) => Eq(arg1, arg2);

let (>::) = (arg1: expr('a), arg2: expr('a)) => Gt(arg1, arg2);

let (<::) = (arg1: expr('a), arg2: expr('a)) => Lt(arg1, arg2);

module type DslType = {type tpe; let wasmType: Wasm.tpe;};

module type DslTypeSpec = {type tpe; let wasmType: Wasm.tpe;};

module DslTypeMake = (Spec: DslTypeSpec) : (DslType with type tpe = Spec.tpe) => {
  type tpe = Spec.tpe;
  let wasmType = Spec.wasmType;
};

module DslInt =
  DslTypeMake(
    {
      type tpe = int;
      let wasmType = Wasm.I32;
    },
  );

/* Modules with functors to make a typed interface for functions */
/* Function with 1 argument */
module F1 = {
  /* DeclInst to be returned from Decl functor */
  module type DeclInst = {
    type a1;
    type ret;
    let a1: expr(a1);
    let call: expr(a1) => expr(ret);
  };
  /* Decl functor is first pass for function definition, and
     includes some helpful code for FuncSpec, variables
     for arguments, and recursive call helper */
  module Decl =
         (A1: DslType, Ret: DslType)
         : (DeclInst with type a1 = A1.tpe and type ret = Ret.tpe) => {
    type a1 = A1.tpe;
    type ret = Ret.tpe;
    let a1: expr(A1.tpe) = Local("a1");
    /* Recursive call */
    let call = (a1: expr(A1.tpe)) : expr(Ret.tpe) => Recurse1(a1);
  };
  /* Funcspec to be passed to make */
  module type FuncSpec = {
    include DeclInst;
    let ident: string;
    let impl: expr(ret);
  };
  /* Function instance, meant to be usable in dsl usage */
  module type Inst = {include FuncSpec; let getImpl: unit => expr(ret);};
  /* Make function instance */
  module Make =
         (Spec: FuncSpec)
         : (Inst with type a1 := Spec.a1 and type ret := Spec.ret) => {
    include Spec;
    let wasmFunc = ref(None);
    let getWasmFunc = () =>
      switch (wasmFunc^) {
      | Some(f) => f
      | None =>
        let f = Wasm.func(ident, [], [], [], [], true);
        wasmFunc := Some(f);
        f;
      };
    let call = (a1: expr(Spec.a1)) : expr(Spec.ret) =>
      Call1(getWasmFunc(), a1);
    let getImpl = () => impl;
  };
};