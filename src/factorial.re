open Dsl;

module Decl = {
  let ident = "factorial";
  type a1 = int;
  type ret = int;
};

module F =
  F1.Make(
    {
      include F1.Decl(Decl);
      let impl = If(a1 <:: Int(1), Int(1), Mul(a1, call(a1 -:: Int(1))));
    },
  );