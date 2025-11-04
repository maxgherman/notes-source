// Domain: string, Codomain: number
// But what are ALL the possible functions?

const parse: (s: string) => number = s => Number(s);
const getLength: (s: string) => number = s => s.length;
const getCharCode: (s: string) => number = s => s.charCodeAt(0);
const alwaysZero: (s: string) => number = s => 0;

// The hom-set Hom(string, number) contains ALL of these and infinitely more!