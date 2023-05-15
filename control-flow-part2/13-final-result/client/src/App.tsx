import { useState } from 'react'
import './App.css'
import { useActors } from './hooks/useActors'

function App() {
  const [x, setX] = useState<string>('');
  const [y, setY] = useState<string>('');
  const { value, errors, sendMessage } = useActors();
  
  const onClick = () => {
    sendMessage({x, y});
  }

  const xError = errors.find(item => item.field === 'x');
  const yError = errors.find(item => item.field === 'y');
  const generalError = errors.find(item => item.field === undefined);

  return (
    <>
      <h2>Division</h2>
      <div className="card">
        <label>x</label>
        <input value={x} onChange={(e) => setX(e.target.value)} />
        <div className="error">
          <h5>{xError?.error || ' '}</h5>
        </div>
      </div>
      <div className="card">
        <label>y</label>
        <input value={y} onChange={(e) => setY(e.target.value)} />
        <div className="error">
          <h5>{yError?.error || ' '}</h5>
        </div>
      </div>

      <div className="error">
          <h5>{generalError?.error || ' '}</h5>
      </div>
      
      <div className="card">
        <button onClick={onClick}>
          Result is {value}
        </button>
      </div>
    </>
  )
}

export default App
