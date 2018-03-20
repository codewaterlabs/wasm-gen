/* https://en.wikipedia.org/wiki/LEB128 */
/* Encode unsigned int */
let encodeU = num => {
  let rec bytes = rest => {
    /* Get 7 bits */
    let part = rest land 0b01111111;
    let rest = rest asr 7;
    if (rest > 0) {
      [part lxor 0b10000000, ...bytes(rest)];
    } else {
      [part];
    };
  };
  bytes(num);
};

let numBits = num => {
  /* Not the most efficient */
  let rec loopPositive = (rest, bits) =>
    if (rest == 0) {
      bits;
    } else {
      loopPositive(rest asr 1, bits + 1);
    };
  let rec loopNegative = (rest, bits) =>
    if (rest == (-1)) {
      bits;
    } else {
      loopNegative(rest asr 1, bits + 1);
    };
  if (num < 0) {
    loopNegative(num, 0);
  } else {
    loopPositive(num, 0);
  };
};

/* Encode signed int */
let encodeS = num => {
  let rec bytes = (rest, bitsLeft) => {
    let part = rest land 0b01111111;
    let rest = rest asr 7;
    if (bitsLeft > 7) {
      [part lxor 0b10000000, ...bytes(rest, bitsLeft - 7)];
    } else {
      [part];
    };
  };
  /* Go for numbits + 1 for sign */
  bytes(num, numBits(num) + 1);
};
