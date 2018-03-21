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

Js.log(ToSexpr.modul(m));

let (size, bin) = ToBin.modul(m);

/*Wasm.printArr(bin, size);*/
let instance = JsWasm.(instance(modul(bin)));

type exports;

[@bs.get] external exports : JsWasm.wasmInstance => exports = "exports";

[@bs.send] external addTwo : (exports, int, int) => int = "addTwo";

let ex = exports(instance);

Js.log(addTwo(ex, 1, 2));

/*let t = Factorial.Func.call(Int(2));*/
Js.log(Factorial.F.getImpl());

let t = Dsl.Eq(Int(1), Factorial.F.getImpl());