const h = require('virtual-dom/h');
import { renderh } from './renderh'

const initialTree = h('div', null, ["Hello! ",
                    h('br'),
                    h('input', {type: "text", value: ''}),
                    h('button', null, ["Click me"])
                  ]);

renderh(initialTree);
