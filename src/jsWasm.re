type wasmInstance;

type wasmModule;

[@bs.new] [@bs.scope "WebAssembly"]
external instance : wasmModule => wasmInstance = "Instance";

[@bs.new] [@bs.scope "WebAssembly"]
external modul : TArray.u8Arr => wasmModule = "Module";
