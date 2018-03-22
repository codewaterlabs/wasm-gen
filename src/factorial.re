open Dsl;

module F =
  F1.Make(
    {
      include F1.Decl(DslInt, DslInt);
      let ident = "factorial";
      let impl = If(a1 <:: Int(1), Int(1), Mul(a1, call(a1 -:: Int(1))));
    },
  );