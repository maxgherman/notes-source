// Alternative-like validation in TypeScript

interface Validation<E, A> {
  readonly _tag: 'Success' | 'Failure';
  readonly value?: A;
  readonly errors?: E[];
}

const success = <E, A>(value: A): Validation<E, A> => ({
  _tag: 'Success',
  value
});

const failure = <E, A>(error: E): Validation<E, A> => ({
  _tag: 'Failure',
  errors: [error]
});

// Alternative operations
const empty = <E, A>(): Validation<E, A> => ({
  _tag: 'Failure',
  errors: []
});

const alt = <E, A>(
  first: Validation<E, A>,
  second: Validation<E, A>
): Validation<E, A> => {
  if (first._tag === 'Success') return first;
  if (second._tag === 'Success') return second;

  return {
    _tag: 'Failure',
    errors: [...(first.errors || []), ...(second.errors || [])]
  };
};

// Validation functions
const validateEmail = (email: string): Validation<string, string> => {
  if (email.includes('@')) {
    return success(email);
  }
  return failure('Invalid email format');
};

const validatePhone = (phone: string): Validation<string, string> => {
  if (/^\d{10}$/.test(phone)) {
    return success(phone);
  }
  return failure('Phone must be 10 digits');
};

const validateURL = (url: string): Validation<string, string> => {
  if (url.startsWith('http://') || url.startsWith('https://')) {
    return success(url);
  }
  return failure('URL must start with http:// or https://');
};

// Contact validation with multiple alternatives
interface Contact {
  name: string;
  contact: string;
}

const validateContact = (name: string, contact: string): Validation<string, Contact> => {
  const contactValidation = alt(
    alt(validateEmail(contact), validatePhone(contact)),
    validateURL(contact)
  );

  if (contactValidation._tag === 'Success') {
    return success({ name, contact: contactValidation.value! });
  }

  return {
    _tag: 'Failure',
    errors: contactValidation.errors || ['No valid contact method']
  };
};

// Usage examples
console.log(validateContact('John', 'john@email.com'));
// Success: { name: 'John', contact: 'john@email.com' }

console.log(validateContact('Jane', '1234567890'));
// Success: { name: 'Jane', contact: '1234567890' }

console.log(validateContact('Bob', 'https://bob.dev'));
// Success: { name: 'Bob', contact: 'https://bob.dev' }

console.log(validateContact('Invalid', 'bad-contact'));
// Failure: errors about email, phone, and URL formats

// Define union type for parsed data
type ParsedData = number | boolean | Record<string, unknown>;

// Multiple validation attempts
const tryParseData = (input: string): Validation<string, ParsedData> => {
  const asJSON = (() => {
    try {
      return success<string, ParsedData>(JSON.parse(input) as ParsedData);
    } catch {
      return failure<string, ParsedData>('Not valid JSON');
    }
  })();

  const asNumber = (() => {
    const num = Number(input);
    return isNaN(num) ? failure<string, ParsedData>('Not a valid number') : success<string, ParsedData>(num);
  })();

  const asBoolean = (() => {
    if (input === 'true') return success<string, ParsedData>(true);
    if (input === 'false') return success<string, ParsedData>(false);
    return failure<string, ParsedData>('Not a valid boolean');
  })();

  return alt(alt(asJSON, asNumber), asBoolean);
};

console.log(tryParseData('{"key":"value"}')); // Success: object
console.log(tryParseData('42'));              // Success: 42
console.log(tryParseData('true'));            // Success: true
console.log(tryParseData('invalid'));         // Failure: all three errors
