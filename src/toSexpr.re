open Wasm;

let instr = instr =>
  switch (instr) {
  | GetLocal(name) => "get_local $" ++ name
  | I32Const(c) => "i32.const " ++ string_of_int(c)
  | I32Add => "i32.add "
  };

let tpe = tpe =>
  switch (tpe) {
  | I32 => "i32"
  | I64 => "i64"
  | F32 => "f32"
  | F64 => "f64"
  | AnyFunc => "anyfunc"
  | Func => "func"
  | Void => "void"
  };

let func = func => {
  let s = "  (func $" ++ func.name ++ " ";
  /* Params */
  let s =
    L.reduceU(func.params, s, (. s, p: local) =>
      s ++ "(param $" ++ p.name ++ " " ++ tpe(p.tpe) ++ ") "
    );
  /* Returns (in wasm currently only one return) */
  let s =
    L.reduceU(func.returns, s, (. s, r: tpe) =>
      s ++ "(result " ++ tpe(r) ++ ") "
    )
    ++ "\n";
  /* Instructions */
  let s =
    L.reduceU(func.instr, s, (. s, inst) =>
      s ++ "    " ++ instr(inst) ++ "\n"
    );
  s ++ "  )\n";
};

let modul = modul => {
  let s = "(module\n";
  let (s, e) =
    L.reduceU(modul.funcs, (s, ""), (. (s, e), f) =>
      (
        s ++ func(f),
        if (f.export) {
          e ++ "  (export \"" ++ f.name ++ "\" (func $" ++ f.name ++ "))\n";
        } else {
          e;
        },
      )
    );
  s ++ e ++ ")\n";
};