import { renderh } from './renderh'
import { Dependency } from './dependency';

export const Library = {
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