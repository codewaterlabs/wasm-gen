type buf;

type i32Arr;

type i8Arr;

type u8Arr;

[@bs.new] external makeBuffer : int => buf = "ArrayBuffer";

/* I32 */
[@bs.new] external makei32 : buf => i32Arr = "Int32Array";

external __i32ToArr : i32Arr => array(int) = "%identity";

let geti32 = (arr, idx) => Belt.Array.getUnsafe(__i32ToArr(arr), idx);

let seti32 = (arr, idx, v) => Belt.Array.setUnsafe(__i32ToArr(arr), idx, v);

/* I8 */
[@bs.new] external makei8 : buf => i8Arr = "Int8Array";

external __i8ToArr : i8Arr => array(int) = "%identity";

let geti8 = (arr, idx) => Belt.Array.getUnsafe(__i8ToArr(arr), idx);

let seti8 = (arr, idx, v) => Belt.Array.setUnsafe(__i8ToArr(arr), idx, v);

/* U8 */
[@bs.new] external makeu8 : buf => u8Arr = "Uint8Array";

/* Make with specified offset, length */
[@bs.new] external makeu8Spec : (buf, int, int) => u8Arr = "Uint8Array";

external __u8ToArr : u8Arr => array(int) = "%identity";

let getu8 = (arr, idx) => Belt.Array.getUnsafe(__u8ToArr(arr), idx);

let setu8 = (arr, idx, v) => Belt.Array.setUnsafe(__u8ToArr(arr), idx, v);