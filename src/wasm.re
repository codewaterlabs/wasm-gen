/* https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md */
type tpe =
  | I32
  | I64
  | F32
  | F64
  | AnyFunc
  | Func
  | Void;

type instr =
  | GetLocal(string)
  | I32Add
  | I32Const(int);

type local = {
  name: string,
  tpe,
};

module HS = Belt.HashMap.String;

module L = Belt.List;

type func = {
  name: string,
  params: list(local),
  returns: list(tpe),
  locals: list(local),
  localIdx: HS.t(int),
  instr: list(instr),
  export: bool,
  numParams: int,
  numLocals: int,
  numReturns: int,
};

type modul = {funcs: list(func)};

let param = (name, tpe) => {name, tpe};

let func = (name, params, returns, locals, instr, export) => {
  let numParams = L.length(params);
  let numLocals = L.length(locals);
  /* Initializing localIdx here.. */
  let localIdx = HS.make(~hintSize=numParams + numLocals);
  let i =
    L.reduceU(
      params,
      0,
      (. i, p: local) => {
        HS.set(localIdx, p.name, i);
        i + 1;
      },
    );
  let _ =
    L.reduceU(
      locals,
      i,
      (. i, l: local) => {
        HS.set(localIdx, l.name, i);
        i + 1;
      },
    );
  {
    name,
    params,
    returns,
    locals,
    localIdx,
    instr,
    export,
    numParams,
    numReturns: L.length(returns),
    numLocals,
  };
};

let modul = funcs : modul => {funcs: funcs};