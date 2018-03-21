open Jest;

describe("Basic sexpr", () =>
  Expect.(
    test("minimal", () => {
      let m = Wasm.(modul([]));
      expect(ToSexpr.modul(m)) |> toBe("(module\n)\n");
    })
  )
);