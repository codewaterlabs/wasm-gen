open Wasm;

module L = Belt.List;

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
      setu8(
        a,
        idx + 1,
        switch (HS.get(func.localIdx, n)) {
        | Some(i) => i
        | None => failwith("Could not find local " ++ n)
        },
      );
      idx + 2;
    | I32Add =>
      setu8(a, idx, 0x6a);
      idx + 1;
    | I32Const(i) =>
      setu8(a, idx, 0x41);
      let idx =
        L.reduceU(
          Leb128.encodeS(i),
          idx + 1,
          (. idx, b) => {
            setu8(a, idx, b);
            idx + 1;
          },
        );
      idx;
    }
  );

let modul = modul => {
  let numFuncs = L.length(modul.funcs);
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
    L.reduceU(
      modul.funcs,
      11,
      (. idx, f) => {
        /* Func type specifier */
        setu8(a, idx, 0x60);
        /* Params array */
        setu8(a, idx + 1, f.numParams);
        let idx =
          L.reduceU(
            f.params,
            idx + 2,
            (. idx, p) => {
              setu8(a, idx, typeCode(p.tpe));
              idx + 1;
            },
          );
        /* Returns array */
        setu8(a, idx, f.numReturns);
        L.reduceU(
          f.returns,
          idx + 1,
          (. idx, tpe) => {
            setu8(a, idx, typeCode(tpe));
            idx + 1;
          },
        );
      },
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
    L.reduceU(
      modul.funcs,
      (idx + 3, 0),
      (. (idx, i), _f) => {
        setu8(a, idx, i);
        (idx + 1, i + 1);
      },
    );
  /* Export section */
  setu8(a, idx, 0x07);
  let exportSizeIdx = idx + 1;
  let (idx, _i, numExports) =
    L.reduceU(modul.funcs, (idx + 2, 0, 0), (. (idx, i, numExports), f) =>
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
      }
    );
  /* Set export section size */
  setu8(a, exportSizeIdx, idx - exportSizeIdx - 1);
  setu8(a, exportSizeIdx + 1, numExports);
  /* Code section */
  setu8(a, idx, 0x0a);
  let codeSizeIdx = idx + 1;
  setu8(a, idx + 2, numFuncs);
  let idx =
    L.reduceU(
      modul.funcs,
      idx + 3,
      (. idx, f) => {
        let numLocals = L.length(f.locals);
        let funcSizeIdx = idx;
        /* todo: write locals */
        setu8(a, idx + 1, numLocals);
        let idx =
          L.reduceU(f.instr, idx + 2, (. idx, instr) =>
            writeInstr(a, f, instr, idx)
          );
        setu8(a, funcSizeIdx, idx - funcSizeIdx);
        /* End function */
        setu8(a, idx, 0x0b);
        idx + 1;
      },
    );
  setu8(a, codeSizeIdx, idx - codeSizeIdx - 1);
  buf.size = idx;
  (buf.size, makeu8Spec(buf.buf, 0, buf.size));
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