import { Dependency } from './dependency';

const Library = {
    createClass(obj) {
      const keyDependencies = {};
      const props = obj.properties;
  
      const newProps = new Proxy(props, {
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
            return true;
          }
      });
  
      obj.properties = newProps;
      obj.render = obj.render.bind(obj);
  
      Dependency.action(obj.render);
  
      return obj;
    }
}
  
const obj = Library.createClass({
    properties: {
        one: 1,
        two: '2'
    },
    render() {
        let value = this.properties.one;

        let placeholder = document.getElementById('placeholder');
        if(placeholder) {
            placeholder.innerHTML = value;
        }
        value = this.properties.two;
        placeholder = document.getElementById('placeholder2');
        if(placeholder) {
            placeholder.innerHTML = value;
        }
    }
});
  
function run() {
    [...Array(10).keys()].forEach(item =>
        setTimeout(() => {
            if (item < 5)
                obj.properties.one = item;
            else
                obj.properties.two = item;
        }, item * 1000));
}
  
run();