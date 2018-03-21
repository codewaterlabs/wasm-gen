open Jest;

describe("Basic sexpr", () =>
  Expect.(
    test("minimal", () => {
      let m = Wasm.(modul([]));
      expect(Wasm.ToSexp.modul(m)) |> toBe("(module\n)\n");
    })
  )
);