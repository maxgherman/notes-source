// Configuration using exponential objects (function injection)
interface Logger {
  log: (message: string) => void;        // Exponential
  error: (error: Error) => void;         // Exponential
}

interface DatabaseClient {
  query: <T>(sql: string) => Promise<T>; // Exponential
  transaction: <T>(fn: (client: DatabaseClient) => Promise<T>) => Promise<T>; // Higher-order
}

type User = { id : string }

type UserData = User

type ValidationResult<T> = {
    user?: T;
    valid: boolean;
}

// Result is a coproduct capturing success or error
type Result<T, E> =
  | { success: true; data: T }
  | { success: false; error: E };

// Minimal app configuration for dependency injection
interface DbConfig {
  connectionString: string;
}

interface LoggingConfig {
  level: 'debug' | 'info' | 'warn' | 'error';
}

interface ValidationConfig {
  // Example rule: require non-empty id
  requireNonEmptyId: boolean;
}

interface AppConfig {
  db: DbConfig;
  logging: LoggingConfig;
  validation: ValidationConfig;
}

// Dependency injection using exponentials
class UserService {
  constructor(
    private db: DatabaseClient,
    private logger: Logger,
    private validator: (user: User) => ValidationResult<User> // Exponential injection
  ) {}

  async createUser(userData: UserData): Promise<Result<User, Error>> {
    try {
      // Use injected exponential (validator function)
      const validation = this.validator(userData);

      if (!validation.valid) {
        return { success: false, error: new Error('Validation failed') };
      }

      // Use injected database exponentials
      const user = await this.db.query<User>(
        'INSERT INTO users ... RETURNING *'
      );

      // Use injected logger exponential
      this.logger.log(`User created: ${user.id}`);

      return { success: true, data: user };
    } catch (error) {
      this.logger.error(error as Error);
      return { success: false, error: error as Error };
    }
  }
}

// Factory function returns configured service (exponential)
const createUserService = (config: AppConfig): UserService =>
  new UserService(
    createDatabaseClient(config.db),
    createLogger(config.logging),
    createUserValidator(config.validation)
  );

// Implementations for dependencies used by the factory
const createDatabaseClient = (_: DbConfig): DatabaseClient => {
  return {} as unknown as DatabaseClient;
};

const createLogger = (_: LoggingConfig): Logger => {
  return {} as unknown as Logger;
};

const createUserValidator = (_: ValidationConfig) =>
  (user: User): ValidationResult<User> => {
    return { valid: true, user };
  };
