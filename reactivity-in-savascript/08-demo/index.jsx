import { Library } from './library'

Library.createClass({
    properties : {
        cardNumber1: '',
        cardNumber2: '',
        cardNumber3: '',
        cardNumber4: '',
        cardHolder: '',
        expirationMonth: '',
        expirationYear: '',
        ccv: '',
        rotate: false
    },
  
    cardNumber() {
        return [this.properties.cardNumber1,
                this.properties.cardNumber2,
                this.properties.cardNumber3,
                this.properties.cardNumber4 ].join(' ');
    },
  
    expirationDate() {
        return [this.properties.expirationMonth,
                  '/', 
                this.properties.expirationYear ].join(' ');
    },
  
    cardNumberChange(property, event) {
        
        if(event.target.value.length >= 4 &&
           event.code !== 'Backspace' &&
           event.code !== 'Delete') {
            return;
        }
        
        setTimeout(() => {
            this.properties[property] = event.target.value;
        }, 0);
    },
  
    cardHolderChange(property, event) {
        
        setTimeout(() => {
            this.properties[property] = event.target.value;
        }, 0);
    },
  
    expirationChange(property, event) {
        this.properties[property] = event.target.value;
    },
  
    ccvChange(event) {
        
        if(event.target.value.length >= 3 &&
           event.code !== 'Backspace' &&
           event.code !== 'Delete') {
            return;
        }
        
        setTimeout(() => {
            this.properties.ccv = event.target.value;
        }, 0);
    },
  
    ccvFocus() {
        this.properties.rotate = true;
    },
  
    ccvBlur() {
        this.properties.rotate = false;
    },
  
    submit(event) {
        event.preventDefault();
        return false;
    },
    
    render() { 
      
      const hover = this.properties.rotate === true ? 'hover' : '';
      const cardBox = ['credit-card-box', hover].join(' ');  
      
      return (
        <div className="checkout">
            <div className={cardBox}>
                <div className="flip">
                  <div className="front">
                    <div className="chip"></div>
                    <div className="logo">
                        <svg version="1.1" id="visa" xmlns="http://www.w3.org/2000/svg" x="0px" y="0px"
                            width="47.834px" height="47.834px" viewBox="0 0 47.834 47.834"
                            style={{enableBackground: 'new 0 0 47.834 47.834'}}>
                            <g>
                            <g>
                                <path d="M44.688,16.814h-3.004c-0.933,0-1.627,0.254-2.037,1.184l-5.773,13.074h4.083c0,0,0.666-1.758,0.817-2.143
                                        c0.447,0,4.414,0.006,4.979,0.006c0.116,0.498,0.474,2.137,0.474,2.137h3.607L44.688,16.814z M39.893,26.01
                                        c0.32-0.819,1.549-3.987,1.549-3.987c-0.021,0.039,0.317-0.825,0.518-1.362l0.262,1.23c0,0,0.745,3.406,0.901,4.119H39.893z
                                        M34.146,26.404c-0.028,2.963-2.684,4.875-6.771,4.875c-1.743-0.018-3.422-0.361-4.332-0.76l0.547-3.193l0.501,0.228
                                        c1.277,0.532,2.104,0.747,3.661,0.747c1.117,0,2.313-0.438,2.325-1.393c0.007-0.625-0.501-1.07-2.016-1.77
                                        c-1.476-0.683-3.43-1.827-3.405-3.876c0.021-2.773,2.729-4.708,6.571-4.708c1.506,0,2.713,0.31,3.483,0.599l-0.526,3.092
                                        l-0.351-0.165c-0.716-0.288-1.638-0.566-2.91-0.546c-1.522,0-2.228,0.634-2.228,1.227c-0.008,0.668,0.824,1.108,2.184,1.77
                                        C33.126,23.546,34.163,24.783,34.146,26.404z M0,16.962l0.05-0.286h6.028c0.813,0.031,1.468,0.29,1.694,1.159l1.311,6.304
                                        C7.795,20.842,4.691,18.099,0,16.962z M17.581,16.812l-6.123,14.239l-4.114,0.007L3.862,19.161
                                        c2.503,1.602,4.635,4.144,5.386,5.914l0.406,1.469l3.808-9.729L17.581,16.812L17.581,16.812z M19.153,16.8h3.89L20.61,31.066
                                        h-3.888L19.153,16.8z"/>
                            </g>
                            </g>
                        </svg>
                    </div>
                    <div className="number">{this.cardNumber()}</div>
                    <div className="card-holder">
                            <label>Card holder</label>
                        <div>{this.properties.cardHolder}</div>
                    </div>
                    <div className="card-expiration-date">
                        <label>Expires</label>
                        <div>{this.expirationDate()}</div>
                    </div>
                  </div>
                  <div className="back">
                      <div className="strip"></div>
                      <div className="logo">
                        <svg version="1.1" id="visa" xmlns="http://www.w3.org/2000/svg" x="0px" y="0px"
                            width="47.834px" height="47.834px" viewBox="0 0 47.834 47.834" 
                            style={{enableBackground : 'new 0 0 47.834 47.834'}}>
                            <g>
                            <g>
                                <path d="M44.688,16.814h-3.004c-0.933,0-1.627,0.254-2.037,1.184l-5.773,13.074h4.083c0,0,0.666-1.758,0.817-2.143
                                        c0.447,0,4.414,0.006,4.979,0.006c0.116,0.498,0.474,2.137,0.474,2.137h3.607L44.688,16.814z M39.893,26.01
                                        c0.32-0.819,1.549-3.987,1.549-3.987c-0.021,0.039,0.317-0.825,0.518-1.362l0.262,1.23c0,0,0.745,3.406,0.901,4.119H39.893z
                                        M34.146,26.404c-0.028,2.963-2.684,4.875-6.771,4.875c-1.743-0.018-3.422-0.361-4.332-0.76l0.547-3.193l0.501,0.228
                                        c1.277,0.532,2.104,0.747,3.661,0.747c1.117,0,2.313-0.438,2.325-1.393c0.007-0.625-0.501-1.07-2.016-1.77
                                        c-1.476-0.683-3.43-1.827-3.405-3.876c0.021-2.773,2.729-4.708,6.571-4.708c1.506,0,2.713,0.31,3.483,0.599l-0.526,3.092
                                        l-0.351-0.165c-0.716-0.288-1.638-0.566-2.91-0.546c-1.522,0-2.228,0.634-2.228,1.227c-0.008,0.668,0.824,1.108,2.184,1.77
                                        C33.126,23.546,34.163,24.783,34.146,26.404z M0,16.962l0.05-0.286h6.028c0.813,0.031,1.468,0.29,1.694,1.159l1.311,6.304
                                        C7.795,20.842,4.691,18.099,0,16.962z M17.581,16.812l-6.123,14.239l-4.114,0.007L3.862,19.161
                                        c2.503,1.602,4.635,4.144,5.386,5.914l0.406,1.469l3.808-9.729L17.581,16.812L17.581,16.812z M19.153,16.8h3.89L20.61,31.066
                                        h-3.888L19.153,16.8z"/>
                            </g>
                            </g>
                        </svg>
                      </div>
                      <div className="ccv">
                        <label>CCV</label>
                        <div>{this.properties.ccv}</div>
                      </div>
                  </div>
                </div>
            </div>
            <form className="form" autoComplete="off" noValidate>
                  <fieldset>
                  <label htmlFor="card-number">Card Number</label>
                  <input type="num" 
                      id="card-number"
                      className="input-cart-number"
                      maxLength="4"
                      value={this.properties.cardNumber1}
                      onkeydown={this.cardNumberChange.bind(this, 'cardNumber1')} />
                  
                  <input type="num" 
                      id="card-number-1" 
                      className="input-cart-number"
                      maxLength="4"
                      value={this.properties.cardNumber2}
                      onkeydown={this.cardNumberChange.bind(this, 'cardNumber2')} />
                      
                  <input type="num"
                      id="card-number-2"
                      className="input-cart-number"
                      maxLength="4"
                      value={this.properties.cardNumber3}
                      onkeydown={this.cardNumberChange.bind(this, 'cardNumber3')} />
                  
                  <input type="num"
                      id="card-number-3"
                      className="input-cart-number"
                      maxLength="4" 
                      value={this.properties.cardNumber4}
                      onkeydown={this.cardNumberChange.bind(this, 'cardNumber4')}/>
                  </fieldset>
                  <fieldset>
                  <label htmlFor="card-holder">Card holder</label>
                  <input type="text"
                      id="card-holder"
                      value={this.properties.cardHolder}
                      onkeydown={this.cardHolderChange.bind(this, 'cardHolder')}
                      />
                  </fieldset>
                  <fieldset className="fieldset-expiration">
                  <label htmlFor="card-expiration-month">Expiration date</label>
                  <div className="select">
                      <select id="card-expiration-month" 
                      value={this.properties.expirationMonth}
                      onchange={this.expirationChange.bind(this, 'expirationMonth')}>
                      <option></option>
                      <option>01</option>
                      <option>02</option>
                      <option>03</option>
                      <option>04</option>
                      <option>05</option>
                      <option>06</option>
                      <option>07</option>
                      <option>08</option>
                      <option>09</option>
                      <option>10</option>
                      <option>11</option>
                      <option>12</option>
                      </select>
                  </div>
                  <div className="select">
                      <select id="card-expiration-year"
                      value={this.properties.expirationYear}
                      onchange={this.expirationChange.bind(this, 'expirationYear')}>
                      <option></option>
                      <option>2016</option>
                      <option>2017</option>
                      <option>2018</option>
                      <option>2019</option>
                      <option>2020</option>
                      <option>2021</option>
                      <option>2022</option>
                      <option>2023</option>
                      <option>2024</option>
                      <option>2025</option>
                      </select>
                  </div>
                  </fieldset>
                  <fieldset className="fieldset-ccv">
                  <label htmlFor="card-ccv">CCV</label>
                  <input type="text"
                      id="card-ccv"
                      maxLength="3"
                      value={this.properties.ccv}
                      onkeydown={this.ccvChange.bind(this)}
                      onfocus={this.ccvFocus.bind(this)}
                      onblur={this.ccvBlur.bind(this)} />
                  </fieldset>
                  <button className="btn" onclick={this.submit.bind(this)}>
                      <i className="fa fa-lock">
                      </i> submit
                  </button>
              </form>
        </div>
      );
    }
});