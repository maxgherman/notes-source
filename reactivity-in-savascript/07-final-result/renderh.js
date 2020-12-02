const diff = require('virtual-dom/diff');
const patch = require('virtual-dom/patch');
const createElement = require('virtual-dom/create-element');

let tree;
let rootNode;

export function renderh(newTree) {

  if(!tree) {
    tree = newTree;
    rootNode = createElement(newTree);
    document.body.appendChild(rootNode);
    return;
  }

  const patches = diff(tree, newTree);
  rootNode = patch(rootNode, patches);
  tree = newTree;
}