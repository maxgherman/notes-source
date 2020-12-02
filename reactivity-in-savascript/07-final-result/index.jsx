import { Library } from './library'

const obj = Library.createClass({
    properties : {
      one : 1,
      two : '2',
      text : ''
    },
  
    onClick() {
      this.properties.text = ' : ' + this.properties.two;
    },
    onChange(event) {
      this.properties.two = event.target.value;
      this.properties.text = ' : ' + this.properties.two;
    },
  
    render() {
  
      return (
          <div>Hello! {this.properties.one}
              <br/>
              <input type="text" value={this.properties.two}
                      onchange={this.onChange.bind(this)} />
              <br/>
              <button onclick={this.onClick.bind(this)}>Click</button>
              <div>{this.properties.text}</div>
          </div>
      );
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
  