function divide(x, y) {
    return y === 0 ?
      { value: null, error: new Error("Division by zero") } :
      { value: x / y, error: null };
  }
  
  const { value, error } = divide(10, 0);
  if (error) {
    console.error(error);
  } else {
    console.log(value);
  }