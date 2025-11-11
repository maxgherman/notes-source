// In programming terms, eval does this:
function _eval<A, B>(func: (a: A) => B, arg: A): B {
  return func(arg);  // Apply function to argument
}

// So if you have:
const add5 = (x: number) => x + 5;  // This is B^A where A=number, B=number
const argument = 3;                 // This is A

_eval(add5, 3);                     // This is B