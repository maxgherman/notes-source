// Domain types
interface User {
  id: number;
  name: string;
  email: string;
}

interface Config {
  dbConnection: string;
  maxRetries: number;
  logLevel: string;
}

type AppError =
  | { type: 'UserNotFound'; id: number }
  | { type: 'DatabaseError'; message: string }
  | { type: 'ValidationError'; message: string };

type LogEntry = string;

// Result type for error handling
interface Result<T, E> {
  map<U>(f: (value: T) => U): Result<U, E>;
  flatMap<U>(f: (value: T) => Result<U, E>): Result<U, E>;
  mapError<F>(f: (error: E) => F): Result<T, F>;
}

// Static factory functions
const Result = {
  ok<T, E>(value: T): Result<T, E> {
    return new Ok(value);
  },

  error<T, E>(error: E): Result<T, E> {
    return new Err(error);
  }
};

class Ok<T, E> implements Result<T, E> {
  constructor(private value: T) {}

  map<U>(f: (value: T) => U): Result<U, E> {
    return new Ok(f(this.value));
  }

  flatMap<U>(f: (value: T) => Result<U, E>): Result<U, E> {
    return f(this.value);
  }

  mapError<F>(f: (error: E) => F): Result<T, F> {
    return new Ok(this.value);
  }

  unwrap(): T { return this.value; }
  toString(): string { return `Ok(${this.value})`; }
}

class Err<T, E> implements Result<T, E> {
  constructor(private error: E) {}

  map<U>(f: (value: T) => U): Result<U, E> {
    return new Err(this.error);
  }

  flatMap<U>(f: (value: T) => Result<U, E>): Result<U, E> {
    return new Err(this.error);
  }

  mapError<F>(f: (error: E) => F): Result<T, F> {
    return new Err(f(this.error));
  }

  getError(): E { return this.error; }
  toString(): string { return `Err(${JSON.stringify(this.error)})`; }
}

// Our monad transformer: ReaderT + ExceptT + WriterT + Promise
class AppM<T> {
  constructor(
    private computation: (config: Config) => Promise<Result<[T, LogEntry[]], AppError>>
  ) {}

  // Run the computation
  async run(config: Config): Promise<Result<[T, LogEntry[]], AppError>> {
    return this.computation(config);
  }

  // Functor: map over the success value
  map<U>(f: (value: T) => U): AppM<U> {
    return new AppM(async (config) => {
      const result = await this.computation(config);
      return result.map(([value, logs]) => [f(value), logs]);
    });
  }

  // Monad: flatMap for chaining operations
  flatMap<U>(f: (value: T) => AppM<U>): AppM<U> {
    return new AppM(async (config) => {
      const result = await this.computation(config);

      if (result instanceof Err) {
        return result as any;
      }

      const [value, logs1] = (result as Ok<[T, LogEntry[]], AppError>).unwrap();
      const nextResult = await f(value).computation(config);

      return nextResult.map(([nextValue, logs2]) => [nextValue, [...logs1, ...logs2]]);
    });
  }

  // Static constructors
  static pure<T>(value: T): AppM<T> {
    return new AppM(async () => Result.ok([value, []]));
  }

  static ask(): AppM<Config> {
    return new AppM(async (config) => Result.ok([config, []]));
  }

  static tell(message: LogEntry): AppM<void> {
    return new AppM(async () => Result.ok([undefined, [message]]));
  }

  static throwError<T>(error: AppError): AppM<T> {
    return new AppM(async () => Result.error(error));
  }

  static liftIO<T>(operation: () => Promise<T>): AppM<T> {
    return new AppM(async () => {
      try {
        const result = await operation();
        return Result.ok([result, []]);
      } catch (error) {
        return Result.error({ type: 'DatabaseError', message: String(error) });
      }
    });
  }
}

// Business operations
const validateUser = (user: User): AppM<User> => {
  return AppM.tell(`Validating user: ${user.name}`)
    .flatMap(() => AppM.ask())
    .flatMap(config => {
      if (user.name.length < 3) {
        return AppM.throwError({ type: 'ValidationError', message: 'Username too short' });
      } else {
        return AppM.tell('User validation passed')
          .flatMap(() => AppM.pure(user));
      }
    });
};

const saveUser = (user: User): AppM<User> => {
  return AppM.ask()
    .flatMap(config =>
      AppM.tell(`Saving user to: ${config.dbConnection}`)
        .flatMap(() => AppM.liftIO(async () => {
          console.log(`Connecting to: ${config.dbConnection}`);
          // Simulate async operation
          await new Promise(resolve => setTimeout(resolve, 100));
          return user;
        }))
        .flatMap(() => {
          if (user.id === 999) {
            return AppM.throwError({ type: 'DatabaseError', message: 'Database connection failed' });
          } else {
            return AppM.tell('User saved successfully')
              .flatMap(() => AppM.pure(user));
          }
        })
    );
};

const findUser = (id: number): AppM<User> => {
  return AppM.tell(`Looking up user with ID: ${id}`)
    .flatMap(() => {
      if (id === 1) {
        return AppM.pure({ id: 1, name: 'alice', email: 'alice@example.com' });
      } else {
        return AppM.throwError({ type: 'UserNotFound', id });
      }
    });
};

const processUser = (id: number, name: string, email: string): AppM<User> => {
  return AppM.tell('Starting user processing')
    .flatMap(() => {
      const newUser: User = { id, name, email };
      return validateUser(newUser)
        .flatMap(saveUser)
        .flatMap(user =>
          AppM.tell('User processing completed successfully')
            .flatMap(() => AppM.pure(user))
        );
    });
};

const updateExistingUser = (id: number, newName: string): AppM<User> => {
  return AppM.tell(`Updating user: ${id}`)
    .flatMap(() => findUser(id))
    .flatMap(existingUser => {
      const updatedUser = { ...existingUser, name: newName };
      return validateUser(updatedUser).flatMap(saveUser);
    });
};

// Example usage
async function main() {
  const config: Config = {
    dbConnection: 'postgresql://localhost/mydb',
    maxRetries: 3,
    logLevel: 'INFO'
  };

  console.log('=== Creating new user ===');
  const result1 = await processUser(42, 'bob', 'bob@example.com').run(config);
  if (result1 instanceof Ok) {
    const [user, logs] = result1.unwrap();
    console.log('Logs:');
    logs.forEach(log => console.log(`  ${log}`));
    console.log(`Result: ${JSON.stringify(user)}`);
  } else {
    console.log(`Error: ${result1}`);
  }

  console.log('\n=== Updating existing user ===');
  const result2 = await updateExistingUser(1, 'alice_updated').run(config);
  if (result2 instanceof Ok) {
    const [user, logs] = result2.unwrap();
    console.log('Logs:');
    logs.forEach(log => console.log(`  ${log}`));
    console.log(`Result: ${JSON.stringify(user)}`);
  } else {
    console.log(`Error: ${result2}`);
  }

  console.log('\n=== Error case ===');
  const result3 = await processUser(999, 'baduser', 'bad@example.com').run(config);
  if (result3 instanceof Ok) {
    const [user, logs] = result3.unwrap();
    console.log('Logs:');
    logs.forEach(log => console.log(`  ${log}`));
    console.log(`Result: ${JSON.stringify(user)}`);
  } else {
    console.log('Logs from error case:');
    console.log(`Error: ${result3}`);
  }
}

// Run the example
main().catch(console.error);
