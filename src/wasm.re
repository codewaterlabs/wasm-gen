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

type func = {
  name: string,
  params: list(local),
  returns: list(tpe),
  locals: list(local),
  localIdx: Hashtbl.t(string, int),
  instr: list(instr),
  export: bool,
  numParams: int,
  numLocals: int,
  numReturns: int,
};

type modul = {funcs: list(func)};

let param = (name, tpe) => {name, tpe};

let func = (name, params, returns, locals, instr, export) => {
  let numParams = List.length(params);
  let numLocals = List.length(locals);
  /* Initializing localIdx here.. */
  let localIdx = Hashtbl.create(numParams + numLocals);
  let i =
    List.fold_left(
      (i, p: local) => {
        Hashtbl.add(localIdx, p.name, i);
        i + 1;
      },
      0,
      params,
    );
  let _ =
    List.fold_left(
      (i, l: local) => {
        Hashtbl.add(localIdx, l.name, i);
        i + 1;
      },
      i,
      locals,
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
    numReturns: List.length(returns),
    numLocals,
  };
};

let modul = funcs : modul => {funcs: funcs};

module ToBin = {
  type buf = {
    arr: TArray.u8Arr,
    mutable size: int,
    mutable bufSize: int,
    buf: TArray.buf,
  };
  let makeBuf = bufSize => {
    open TArray;
    let ab = makeBuffer(bufSize);
    let arr = makeu8(ab);
    {arr, size: 0, bufSize, buf: ab};
  };
  let typeCode = tpe =>
    /* These are translated relative to spec to be used as unsigned bytes */
    switch (tpe) {
    | I32 => 0x7f
    | I64 => 0x7e
    | F32 => 0x7d
    | F64 => 0x7c
    | AnyFunc => 0x6f
    | Func => 0x5f
    | Void => 0x3f
    };
  let writeStr = (a, str, idx, strLen) => {
    open TArray;
    setu8(a, idx, strLen);
    let idx = idx + 1;
    let rec loop = i =>
      if (i < strLen) {
        setu8(a, idx + i, int_of_char(String.unsafe_get(str, i)));
        loop(i + 1);
      };
    loop(0);
  };
  let writeInstr = (a, func, instr, idx) =>
    TArray.(
      switch (instr) {
      | GetLocal(n) =>
        setu8(a, idx, 0x20);
        setu8(a, idx + 1, Hashtbl.find(func.localIdx, n));
        idx + 2;
      | I32Add =>
        setu8(a, idx, 0x6a);
        idx + 1;
      | I32Const(i) =>
        setu8(a, idx, 0x41);
        let idx =
          List.fold_left(
            (idx, b) => {
              setu8(a, idx, b);
              idx + 1;
            },
            idx + 1,
            Leb128.encodeS(i),
          );
        idx;
      }
    );
  let modul = modul => {
    let numFuncs = List.length(modul.funcs);
    let estSize = numFuncs * 256;
    let buf = makeBuf(estSize);
    let a = buf.arr;
    open TArray;
    /* Binary magic header, \0asm */
    setu8(a, 0, 0x0);
    setu8(a, 1, 0x61);
    setu8(a, 2, 0x73);
    setu8(a, 3, 0x6d);
    /* Wasm version */
    setu8(a, 4, 0x1);
    /* Type section, this lists function signatures */
    setu8(a, 8, 0x1);
    let typesSizeIdx = 9;
    setu8(a, 10, numFuncs);
    /* Todo: Imports */
    /* For each function, add params and returns specifiers */
    let idx =
      List.fold_left(
        (idx, f) => {
          /* Func type specifier */
          setu8(a, idx, 0x60);
          /* Params array */
          setu8(a, idx + 1, f.numParams);
          let idx =
            List.fold_left(
              (idx, p) => {
                setu8(a, idx, typeCode(p.tpe));
                idx + 1;
              },
              idx + 2,
              f.params,
            );
          /* Returns array */
          setu8(a, idx, f.numReturns);
          List.fold_left(
            (idx, tpe) => {
              setu8(a, idx, typeCode(tpe));
              idx + 1;
            },
            idx + 1,
            f.returns,
          );
        },
        11,
        modul.funcs,
      );
    /* Set types section byte size */
    setu8(a, typesSizeIdx, idx - typesSizeIdx - 1);
    /* Function section, this lists functions found in code section below */
    setu8(a, idx, 0x03);
    /* Section size */
    setu8(a, idx + 1, numFuncs + 1);
    /* Num functions (array len) */
    setu8(a, idx + 2, numFuncs);
    /* Set type index for each function */
    let (idx, _) =
      List.fold_left(
        ((idx, i), _f) => {
          setu8(a, idx, i);
          (idx + 1, i + 1);
        },
        (idx + 3, 0),
        modul.funcs,
      );
    /* Export section */
    setu8(a, idx, 0x07);
    let exportSizeIdx = idx + 1;
    let (idx, _i, numExports) =
      List.fold_left(
        ((idx, i, numExports), f) =>
          if (f.export) {
            /* Write function name to exports */
            let nameLen = String.length(f.name);
            setu8(a, idx, nameLen);
            writeStr(a, f.name, idx + 1, nameLen);
            let idx = idx + nameLen + 2;
            /* Export kind is function (0x0) */
            setu8(a, idx, 0x0);
            /* Function type index */
            setu8(a, idx + 1, i);
            (idx + 2, i + 1, numExports + 1);
          } else {
            (idx, i + 1, numExports);
          },
        (idx + 2, 0, 0),
        modul.funcs,
      );
    /* Set export section size */
    setu8(a, exportSizeIdx, idx - exportSizeIdx - 1);
    setu8(a, exportSizeIdx + 1, numExports);
    /* Code section */
    setu8(a, idx, 0x0a);
    let codeSizeIdx = idx + 1;
    setu8(a, idx + 2, numFuncs);
    let idx =
      List.fold_left(
        (idx, f) => {
          let numLocals = List.length(f.locals);
          let funcSizeIdx = idx;
          /* todo: write locals */
          setu8(a, idx + 1, numLocals);
          let idx =
            List.fold_left(
              (idx, instr) => writeInstr(a, f, instr, idx),
              idx + 2,
              f.instr,
            );
          setu8(a, funcSizeIdx, idx - funcSizeIdx);
          /* End function */
          setu8(a, idx, 0x0b);
          idx + 1;
        },
        idx + 3,
        modul.funcs,
      );
    setu8(a, codeSizeIdx, idx - codeSizeIdx - 1);
    buf.size = idx;
    (buf.size, makeu8Spec(buf.buf, 0, buf.size));
  };
};

module ToSexp = {
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
      List.fold_left(
        (s, p: local) =>
          s ++ "(param $" ++ p.name ++ " " ++ tpe(p.tpe) ++ ") ",
        s,
        func.params,
      );
    /* Returns (in wasm currently only one return) */
    let s =
      List.fold_left(
        (s, r: tpe) => s ++ "(result " ++ tpe(r) ++ ") ",
        s,
        func.returns,
      )
      ++ "\n";
    /* Instructions */
    let s =
      List.fold_left(
        (s, inst) => s ++ "    " ++ instr(inst) ++ "\n",
        s,
        func.instr,
      );
    s ++ "  )\n";
  };
  let modul = modul => {
    let s = "(module\n";
    let (s, e) =
      List.fold_left(
        ((s, e), f) => (
          s ++ func(f),
          if (f.export) {
            e ++ "  (export \"" ++ f.name ++ "\" (func $" ++ f.name ++ "))\n";
          } else {
            e;
          },
        ),
        (s, ""),
        modul.funcs,
      );
    s ++ e ++ ")\n";
  };
};

let printArr = (a, size) => {
  let rec loop = i =>
    if (i < size) {
      Js.log3(
        "adr",
        Utils.hexString(i),
        Utils.hexString(TArray.getu8(a, i)),
      );
      loop(i + 1);
    };
  loop(0);
};