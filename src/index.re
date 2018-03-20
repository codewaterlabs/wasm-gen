let m =
  Wasm.(
    modul([
      func(
        "addTwo",
        [param("i1", I32), param("i2", I32)],
        [I32],
        [],
        [GetLocal("i1"), GetLocal("i2"), I32Add],
        true,
      ),
    ])
  );

Js.log(Wasm.ToSexp.modul(m));

let (size, bin) = Wasm.ToBin.modul(m);

/*Wasm.printArr(bin, size);*/
let instance = JsWasm.(instance(modul(bin)));

type exports;

[@bs.get] external exports : JsWasm.wasmInstance => exports = "exports";

[@bs.send] external addTwo : (exports, int, int) => int = "addTwo";

let ex = exports(instance);

Js.log(addTwo(ex, 1, 2));
