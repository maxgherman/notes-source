// Form data is a product of field values
interface RegistrationForm {
  email: string;        // Component 1
  password: string;     // Component 2
  confirmPassword: string; // Component 3
  acceptTerms: boolean; // Component 4
}

// Validation result is a coproduct
type ValidationResult<T> =
  | { valid: true; data: T }
  | { valid: false; errors: string[] };

// Validation combines projections with case analysis
function validateRegistration(form: RegistrationForm): ValidationResult<User> {
  const errors: string[] = [];

  // Projections extract individual fields for validation
  if (!isValidEmail(form.email)) {
    errors.push('Invalid email format');
  }

  if (form.password.length < 8) {
    errors.push('Password must be at least 8 characters');
  }

  if (form.password !== form.confirmPassword) {
    errors.push('Passwords do not match');
  }

  if (!form.acceptTerms) {
    errors.push('You must accept the terms');
  }

  // Return coproduct result
  if (errors.length === 0) {
    return {
      valid: true,
      data: { email: form.email, hashedPassword: hash(form.password) }
    };
  } else {
    return { valid: false, errors };
  }
}