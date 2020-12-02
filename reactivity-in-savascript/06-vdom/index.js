const h = require('virtual-dom/h');
import { renderh } from './renderh'
import { Dependency } from './dependency';

const Library = {
  createClass(obj) {

    let keyDependencies = {};
    let props = obj.properties;

    let newProps = new Proxy(props, {
        get: function(target, name) {

          let dependency = keyDependencies[name];

          if(!dependency) {
              dependency = Dependency.create();
              keyDependencies[name] = dependency;
          }

          dependency.depend();

          return target[name];

        },
        set: function(obj, prop, value) {
          obj[prop] = value;

          let dependency = keyDependencies[prop];

          if(dependency) {
            dependency.change();
          }
        }
    });

    obj.properties = newProps;

    let render = obj.render.bind(obj);

    obj.render = function() {
      let tree = render();
      renderh(tree);        //  VDOM update
    };

    Dependency.action(obj.render);

    return obj;
  }
}
  
const obj = Library.createClass({
  properties : {
    one : 1,
    two : "2"
  },
  render() {

    return h('div', null, ["Hello! ", this.properties.one,
                h('br'),
                h('input', {type: "text", value: this.properties.two}),
                h('br'),
                h('button', null, ["Click me"])
          ]);
  }
});

[...Array(10).keys()].forEach(item =>{

    setTimeout(() => {
        if(item < 5)
          obj.properties.one = item;
        else
          obj.properties.two = item;

    }, item * 1000);
})
  