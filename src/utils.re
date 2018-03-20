let bitString = (num, numBits) => {
  let rec loop = (rest, bitsLeft) =>
    if (bitsLeft <= 0) {
      "";
    } else {
      let bit = rest land 0b1;
      let str = bit == 0 ? "0" : "1";
      loop(rest asr 1, bitsLeft - 1) ++ str;
    };
  loop(num, numBits);
};

let hexString = byte => {
  let rec loop = rest => {
    let quotient = rest / 16;
    let remainder = rest - quotient * 16;
    let str =
      switch (remainder) {
      | 15 => "f"
      | 14 => "e"
      | 13 => "d"
      | 12 => "c"
      | 11 => "b"
      | 10 => "a"
      | _ => string_of_int(remainder)
      };
    if (quotient == 0) {
      str;
    } else {
      loop(quotient) ++ str;
    };
  };
  "0x" ++ loop(byte);
};
