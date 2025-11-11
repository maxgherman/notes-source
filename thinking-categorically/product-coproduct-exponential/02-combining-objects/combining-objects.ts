// Combining different pieces of information
interface User {
  name: string;
  age: number;
  email: string;
}

// Representing alternatives
type PaymentMethod = 'credit' | 'debit' | 'cash' | 'digital';

type SuccessResult = { numberOdRecords : number }

type ErrorResult = { error: Error } 

// Database query results
type QueryResult = SuccessResult | ErrorResult;
