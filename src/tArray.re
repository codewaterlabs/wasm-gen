type buf;

type i32Arr;

type i8Arr;

type u8Arr;

[@bs.new] external makeBuffer : int => buf = "ArrayBuffer";

/* I32 */
[@bs.new] external makei32 : buf => i32Arr = "Int32Array";

[@bs.get_index] external geti32 : (i32Arr, int) => int = "";

[@bs.set_index] external seti32 : (i32Arr, int, int) => unit = "";

/* I8 */
[@bs.new] external makei8 : buf => i8Arr = "Int8Array";

[@bs.get_index] external geti8 : (i8Arr, int) => int = "";

[@bs.set_index] external seti8 : (i8Arr, int, int) => unit = "";

/* U8 */
[@bs.new] external makeu8 : buf => u8Arr = "Uint8Array";

/* Make with specified offset, length */
[@bs.new] external makeu8Spec : (buf, int, int) => u8Arr = "Uint8Array";

[@bs.get_index] external getu8 : (u8Arr, int) => int = "";

[@bs.set_index] external setu8 : (u8Arr, int, int) => unit = "";