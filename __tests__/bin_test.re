open Jest;

type exports;

[@bs.get] external exports : JsWasm.wasmInstance => exports = "exports";

module Moduls = {
  let addTwo =
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
};

[@bs.send] external addTwo : (exports, int, int) => int = "addTwo";

let modulToExports = m => {
  let (_size, bin) = Wasm.ToBin.modul(m);
  let instance = JsWasm.(instance(modul(bin)));
  exports(instance);
};

describe("Basic bin", () =>
  Expect.(
    test("addTwo", () => {
      let e = modulToExports(Moduls.addTwo);
      expect(addTwo(e, 1, 2)) |> toBe(3);
    })
  )
);