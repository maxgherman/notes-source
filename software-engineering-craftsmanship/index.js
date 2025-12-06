const NIL = Symbol('list.nil');
const nill = () => NIL;

// Checks
console.assert(typeof NIL === 'symbol');
console.assert(nill() === nill(), 'nill must be stable');

const isEmpty = (list) => list === nill();

// Checks
console.assert(isEmpty(nill()));
console.assert(!isEmpty(undefined));

// A pair is [value, next]
const cons = (value, next = nill()) => [value, next];

// Quick check
const one = cons(1);
console.assert(
  Array.isArray(one) &&
  one.length === 2 &&
  one[0] === 1 &&
  one[1] === nill()
);

// Now that cons is defined, we can safely use it in checks
console.assert(!isEmpty(cons(0)));

// Nested construction (right‑associative)
const twoNodes = cons(1, cons(2));      // [1, [2, Symbol(list.nil)]]
const threeNodes = cons('a', cons('b', cons('c')));

console.assert(twoNodes[0] === 1 && twoNodes[1][0] === 2);
console.assert(
  threeNodes[0] === 'a' &&
  threeNodes[1][0] === 'b' &&
  threeNodes[1][1][0] === 'c'
);

// Linking via intermediate variables
const third = cons(3);
const second = cons(2, third);
const first  = cons(1, second);

console.assert(
  first[1] === second &&
  second[1] === third &&
  third[1] === nill()
);

// Prepending step by step (build frontwards)
{
  let xs = nill();
  xs = cons(3, xs); // [3, NIL]
  xs = cons(2, xs); // [2, [3, NIL]]
  xs = cons(1, xs); // [1, [2, [3, NIL]]]

  console.assert(
    xs[0] === 1 &&
    xs[1][0] === 2 &&
    xs[1][1][0] === 3
  );
}

const head = (list) => list === nill() ? undefined : list[0];

// Checks
console.assert(head(cons(42)) === 42);
console.assert(head(nill()) === undefined);

const tail = (list) => list === nill() ? nill() : list[1];

// Checks
const two = cons(2);
const pair = cons(1, two);
console.assert(tail(pair) === two);
console.assert(tail(nill()) === nill());

const foldr = (list, f, z) => {
  const go = (p) => isEmpty(p) ? z : f(head(p), go(tail(p)));
  return go(list);
};

const sum = (list) => foldr(list, (x, acc) => x + acc, 0);

const xs = cons(1, cons(2, cons(3)));
console.assert(sum(xs) === 6);

const mapF = (list, g) =>
  foldr(list, (x, acc) => cons(g(x), acc), nill());

const ys = mapF(cons(1, cons(2, cons(3))), x => x * 2);
const arrY = [];

for (let p = ys; !isEmpty(p); p = tail(p)) {
  arrY.push(head(p));
}

console.assert(JSON.stringify(arrY) === JSON.stringify([2,4,6]));

const filterF = (list, predicate) =>
  foldr(list, (x, acc) => predicate(x) ? cons(x, acc) : acc, nill());

const zs = filterF(cons(1, cons(2, cons(3))), x => x % 2);
const arrZ = [];

for (let p = zs; !isEmpty(p); p = tail(p)) {
  arrZ.push(head(p));
}

console.assert(JSON.stringify(arrZ) === JSON.stringify([1,3]));

const toArray = (list) => {
  const out = [];

  for (let p = list; !isEmpty(p); p = tail(p)) {
    out.push(head(p));
  }

  return out;
};

console.assert(
  JSON.stringify(toArray(cons(1, cons(2, cons(3))))) ===
  JSON.stringify([1,2,3])
);

const toArrayF = (list) => foldr(list, (x, acc) => [x, ...acc], []);

console.assert(
  JSON.stringify(toArrayF(cons(1, cons(2, cons(3))))) ===
  JSON.stringify([1,2,3])
);

const tnode = (left, value, right) => [left, value, right];
const leaf  = (value) => tnode(nill(), value, nill());

const isTreeEmpty = (t) => t === nill();
const leftOf  = (t) => isTreeEmpty(t) ? nill()     : t[0];
const valueOf = (t) => isTreeEmpty(t) ? undefined  : t[1];
const rightOf = (t) => isTreeEmpty(t) ? nill()     : t[2];

// Example tree:    2
//                 / \
//                1   3

const tree = tnode(leaf(1), 2, leaf(3));

console.assert(valueOf(tree) === 2);
console.assert(valueOf(leftOf(tree)) === 1);
console.assert(valueOf(rightOf(tree)) === 3);

// Concatenate two pair-lists
const concat = (xs, ys) => isEmpty(xs) ? ys : cons(head(xs), concat(tail(xs), ys));

// In‑order: left, value, right
const inOrder = (t) =>
  isTreeEmpty(t) ? nill() :
  concat(
    inOrder(leftOf(t)),
    cons(valueOf(t), inOrder(rightOf(t)))
  );

// Pre‑order: value, left, right
const preOrder = (t) =>
  isTreeEmpty(t) ? nill() :
  cons(
    valueOf(t),
    concat(preOrder(leftOf(t)), preOrder(rightOf(t)))
  );

// Post‑order: left, right, value
const postOrder = (t) =>
  isTreeEmpty(t) ? nill() :
  concat(
    postOrder(leftOf(t)),
    concat(postOrder(rightOf(t)), cons(valueOf(t), nill()))
  );

// Demo on   2
//          / \
//         1   3
const t = tnode(leaf(1), 2, leaf(3));

console.assert(
  JSON.stringify(toArray(inOrder(t))) === JSON.stringify([1,2,3])
);
console.assert(
  JSON.stringify(toArray(preOrder(t))) === JSON.stringify([2,1,3])
);
console.assert(
  JSON.stringify(toArray(postOrder(t))) === JSON.stringify([1,3,2])
);

const Q = (front = nill(), back = nill()) => [front, back];
const isQEmpty = ([f, b]) => isEmpty(f) && isEmpty(b);

// Local reverse for lists
const reverse = (list) => {
  let r = nill();

  for (let p = list; !isEmpty(p); p = tail(p)) {
    r = cons(head(p), r);
  }
  return r;
};

const rebalance = ([f, b]) => isEmpty(f) ? Q(reverse(b), nill()) : Q(f, b);

const enqueue = (q, x) => {
  const [f, b] = q;
  return Q(f, cons(x, b));
};

// Dequeue returns a pair [value, nextQueue] or undefined if empty
const dequeue = (q) => {
  const [f0, b0] = rebalance(q);
  if (isEmpty(f0)) return undefined;
  return [head(f0), Q(tail(f0), b0)];
};

// queue -> JS array (front ++ reverse(back))
const toArrayQ = ([f, b]) => {
  const a = [];
  for (let p = f; !isEmpty(p); p = tail(p)) a.push(head(p));
  const br = reverse(b);
  for (let p = br; !isEmpty(p); p = tail(p)) a.push(head(p));
  return a;
};

// Demo
let q = Q();
q = enqueue(q, 1);
q = enqueue(q, 2);
q = enqueue(q, 3);
console.assert(JSON.stringify(toArrayQ(q)) === JSON.stringify([1,2,3]));

let r = dequeue(q); const x1 = r[0]; q = r[1];
console.assert(x1 === 1);
console.assert(JSON.stringify(toArrayQ(q)) === JSON.stringify([2,3]));

q = enqueue(q, 4);
q = enqueue(q, 5);
console.assert(JSON.stringify(toArrayQ(q)) === JSON.stringify([2,3,4,5]));

const setHas = (cmp, x, t) =>
  isTreeEmpty(t) ? false :
  (cmp(x, valueOf(t)) === 0 ? true :
   cmp(x, valueOf(t)) < 0 ? setHas(cmp, x, leftOf(t)) : setHas(cmp, x, rightOf(t)));

const setInsert = (cmp, x, t) => {
  if (isTreeEmpty(t)) return leaf(x);
  const v = valueOf(t);
  if (cmp(x, v) === 0) return t; // no duplicates
  if (cmp(x, v) < 0) return tnode(setInsert(cmp, x, leftOf(t)), v, rightOf(t));
  return tnode(leftOf(t), v, setInsert(cmp, x, rightOf(t)));
};

const minNode = (t) => isTreeEmpty(leftOf(t)) ? t : minNode(leftOf(t));

const setRemove = (cmp, x, t) => {
  if (isTreeEmpty(t)) return t;
  const v = valueOf(t);
  if (cmp(x, v) < 0) return tnode(setRemove(cmp, x, leftOf(t)), v, rightOf(t));
  if (cmp(x, v) > 0) return tnode(leftOf(t), v, setRemove(cmp, x, rightOf(t)));
  // delete this node
  if (isTreeEmpty(leftOf(t)))  return rightOf(t);
  if (isTreeEmpty(rightOf(t))) return leftOf(t);
  // two children: swap with inorder successor
  const m = minNode(rightOf(t));
  return tnode(leftOf(t), valueOf(m), setRemove(cmp, valueOf(m), rightOf(t)));
};

// Demo
const byNumber = (a, b) => a - b;
let s = nill();
s = setInsert(byNumber, 2, s);
s = setInsert(byNumber, 1, s);
s = setInsert(byNumber, 3, s);
console.assert(setHas(byNumber, 2, s));
console.assert(JSON.stringify(toArray(inOrder(s))) === JSON.stringify([1,2,3]));
s = setRemove(byNumber, 2, s);
console.assert(JSON.stringify(toArray(inOrder(s))) === JSON.stringify([1,3]));

const keyOf = (entry) => entry[0];
const valOf = (entry) => entry[1];

const mapLookup = (cmp, k, t) => {
  if (isTreeEmpty(t)) return undefined;
  const e = valueOf(t), c = cmp(k, keyOf(e));
  if (c === 0) return valOf(e);
  return c < 0 ? mapLookup(cmp, k, leftOf(t)) : mapLookup(cmp, k, rightOf(t));
};

const mapInsert = (cmp, k, v, t) => {
  if (isTreeEmpty(t)) return tnode(nill(), [k, v], nill());
  const e = valueOf(t), c = cmp(k, keyOf(e));
  if (c === 0) return tnode(leftOf(t), [k, v], rightOf(t)); // replace
  if (c < 0)   return tnode(mapInsert(cmp, k, v, leftOf(t)), e, rightOf(t));
  return tnode(leftOf(t), e, mapInsert(cmp, k, v, rightOf(t)));
};

const mapRemove = (cmp, k, t) => {
  if (isTreeEmpty(t)) return t;
  const e = valueOf(t), c = cmp(k, keyOf(e));
  if (c < 0) return tnode(mapRemove(cmp, k, leftOf(t)), e, rightOf(t));
  if (c > 0) return tnode(leftOf(t), e, mapRemove(cmp, k, rightOf(t)));
  // delete node
  if (isTreeEmpty(leftOf(t)))  return rightOf(t);
  if (isTreeEmpty(rightOf(t))) return leftOf(t);
  const m = minNode(rightOf(t));
  return tnode(leftOf(t), valueOf(m), mapRemove(cmp, keyOf(valueOf(m)), rightOf(t)));
};

// Demo
let m = nill();
m = mapInsert(byNumber, 2, 'two', m);
m = mapInsert(byNumber, 1, 'one', m);
m = mapInsert(byNumber, 3, 'three', m);
console.assert(mapLookup(byNumber, 2, m) === 'two');
// In‑order over entries; project keys to show order
const keys = []; for (let p = inOrder(m); !isEmpty(p); p = tail(p)) keys.push(keyOf(head(p)));
console.assert(JSON.stringify(keys) === JSON.stringify([1,2,3]));
m = mapRemove(byNumber, 2, m);
const keys2 = []; for (let p = inOrder(m); !isEmpty(p); p = tail(p)) keys2.push(keyOf(head(p)));
console.assert(JSON.stringify(keys2) === JSON.stringify([1,3]));

// Helpers
const contains = (xs, x) => {
  for (let p = xs; !isEmpty(p); p = tail(p)) {
    if (head(p) === x) return true;
  }

 return false;
};


const gNeighbors = (g, v) => {
  for (let p = g; !isEmpty(p); p = tail(p)) {
    const e = head(p);

    if (e[0] === v) return e[1];
  }
  return nill();
};

const gInsertVertex = (g, v) => {
  const go = (p) => {
    if (isEmpty(p)) return cons([v, nill()], nill());
    const e = head(p);
    return (e[0] === v) ? p : cons(e, go(tail(p)));
  };
  return go(g);
};

const gSetNeighbors = (g, v, ns) => {
  if (isEmpty(g)) return cons([v, ns], nill());
  const e = head(g);
  if (e[0] === v) return cons([v, ns], tail(g));
  return cons(e, gSetNeighbors(tail(g), v, ns));
};

// Add a directed edge v -> w (ensures vertices exist)
const gAddEdge = (g, v, w) => {
  let g1 = gInsertVertex(g, v);
  g1 = gInsertVertex(g1, w);
  const ns = gNeighbors(g1, v);
  // Append to preserve insertion order: ns ++ [w]
  const ns1 = contains(ns, w) ? ns : concat(ns, cons(w, nill()));
  return gSetNeighbors(g1, v, ns1);
};

// BFS demo wrapped to avoid name collisions with reverse
{
  const reverse = (list) => { let r = nill(); for (let p = list; !isEmpty(p); p = tail(p)) r = cons(head(p), r); return r; };
  const bfs = (g, start) => {
    const Q = (front = nill(), back = nill()) => [front, back];
    const rebalance = ([f, b]) => isEmpty(f) ? [reverse(b), nill()] : [f, b];
    const enqueue = ([f, b], x) => [f, cons(x, b)];
    const dequeue = (q) => { const [f0, b0] = rebalance(q); return isEmpty(f0) ? undefined : [head(f0), [tail(f0), b0]]; };

    let q = enqueue(Q(), start);
    let seen = nill();
    let orderRev = nill(); // accumulate reversed

    for (let step = dequeue(q); step !== undefined; step = dequeue(q)) {
      const node = step[0]; q = step[1];
      if (!contains(seen, node)) {
        seen = cons(node, seen);
        orderRev = cons(node, orderRev);
        for (let ns = gNeighbors(g, node); !isEmpty(ns); ns = tail(ns)) {
          q = enqueue(q, head(ns));
        }
      }
    }

    return reverse(orderRev);
  };

  // Demo graph: 1 -> 2, 1 -> 3, 2 -> 4
  let G = nill();
  G = gAddEdge(G, 1, 2);
  G = gAddEdge(G, 1, 3);
  G = gAddEdge(G, 2, 4);

  const order = []; for (let p = bfs(G, 1); !isEmpty(p); p = tail(p)) order.push(head(p));
  console.assert(JSON.stringify(order) === JSON.stringify([1,2,3,4]));
}
