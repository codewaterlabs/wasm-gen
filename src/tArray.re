type buf;

type i32Arr;

type i8Arr;

type u8Arr;

[@bs.new] external makeBuffer : int => buf = "ArrayBuffer";

/* I32 */
[@bs.new] external makeI32 : buf => i32Arr = "Int32Array";

[@bs.val] external getI32 : (i32Arr, int) => int = "arrayGet";

[@bs.val] external setI32 : (i32Arr, int, int) => int = "arraySet";

/* I8 */
[@bs.new] external makeI8 : buf => i8Arr = "Int8Array";

[@bs.val] external getI8 : (i8Arr, int) => int = "arrayGet";

[@bs.val] external setI8 : (i8Arr, int, int) => int = "arraySet";

/* U8 */
[@bs.new] external makeU8 : buf => u8Arr = "Uint8Array";

/* Make with specified offset, length */
[@bs.new] external makeU8Spec : (buf, int, int) => u8Arr = "Uint8Array";

[@bs.val] external getU8 : (u8Arr, int) => int = "arrayGet";

[@bs.val] external setU8 : (u8Arr, int, int) => unit = "arraySet";
