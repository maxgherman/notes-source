// MonadPlus-like pattern for async operations with fallbacks

interface AsyncResult<E, A> {
  readonly _tag: 'Success' | 'Failure';
  readonly value?: A;
  readonly error?: E;
}

class AsyncMonad<E, A> {
  constructor(private computation: () => Promise<AsyncResult<E, A>>) {}

  // Monad operations
  static of<E, A>(value: A): AsyncMonad<E, A> {
    return new AsyncMonad(() => Promise.resolve({
      _tag: 'Success',
      value
    }));
  }

  async flatMap<B>(f: (a: A) => AsyncMonad<E, B>): Promise<AsyncMonad<E, B>> {
    const result = await this.computation();
    if (result._tag === 'Failure') {
      return new AsyncMonad(() => Promise.resolve({
        _tag: 'Failure',
        error: result.error
      }));
    }
    return f(result.value!);
  }

  // MonadPlus operations
  static mzero<E, A>(): AsyncMonad<E, A> {
    return new AsyncMonad(() => Promise.resolve({
      _tag: 'Failure',
      error: undefined as unknown as E
    }));
  }

  mplus(other: AsyncMonad<E, A>): AsyncMonad<E, A> {
    return new AsyncMonad(async () => {
      const result = await this.computation();
      if (result._tag === 'Success') {
        return result;
      }
      return other.computation();
    });
  }

  async run(): Promise<AsyncResult<E, A>> {
    return this.computation();
  }
}

// API service interfaces
interface User {
  id: number;
  name: string;
  email: string;
}

interface UserService {
  getUser(id: number): AsyncMonad<string, User>;
}

// Different API implementations
class PrimaryUserService implements UserService {
  getUser(id: number): AsyncMonad<string, User> {
    return new AsyncMonad(async () => {
      try {
        // Simulate primary API call
        await new Promise(resolve => setTimeout(resolve, 100));

        if (Math.random() > 0.7) { // 30% failure rate
          return {
            _tag: 'Success',
            value: { id, name: `User${id}`, email: `user${id}@primary.com` }
          };
        }

        return {
          _tag: 'Failure',
          error: 'Primary service unavailable'
        };
      } catch (error) {
        return {
          _tag: 'Failure',
          error: 'Primary service error'
        };
      }
    });
  }
}

class BackupUserService implements UserService {
  getUser(id: number): AsyncMonad<string, User> {
    return new AsyncMonad(async () => {
      try {
        // Simulate backup API call
        await new Promise(resolve => setTimeout(resolve, 200));

        if (Math.random() > 0.3) { // 70% success rate
          return {
            _tag: 'Success',
            value: { id, name: `BackupUser${id}`, email: `user${id}@backup.com` }
          };
        }

        return {
          _tag: 'Failure',
          error: 'Backup service unavailable'
        };
      } catch (error) {
        return {
          _tag: 'Failure',
          error: 'Backup service error'
        };
      }
    });
  }
}

class CacheUserService implements UserService {
  private cache = new Map<number, User>();

  constructor() {
    // Pre-populate cache
    this.cache.set(1, { id: 1, name: 'CachedUser1', email: 'user1@cache.local' });
    this.cache.set(2, { id: 2, name: 'CachedUser2', email: 'user2@cache.local' });
  }

  getUser(id: number): AsyncMonad<string, User> {
    return new AsyncMonad(async () => {
      const user = this.cache.get(id);
      if (user) {
        return {
          _tag: 'Success',
          value: user
        };
      }

      return {
        _tag: 'Failure',
        error: 'User not found in cache'
      };
    });
  }
}

// Service orchestrator using MonadPlus pattern
class UserServiceOrchestrator {
  constructor(
    private primary: UserService,
    private backup: UserService,
    private cache: UserService
  ) {}

  async getUserWithFallback(id: number): Promise<AsyncResult<string, User>> {
    // MonadPlus chain: try primary, then backup, then cache
    const result = this.primary.getUser(id)
      .mplus(this.backup.getUser(id))
      .mplus(this.cache.getUser(id));

    return result.run();
  }

  async getUserProfile(id: number): Promise<AsyncResult<string, { user: User; profile: string }>> {
    const userMonad = this.primary.getUser(id)
      .mplus(this.backup.getUser(id))
      .mplus(this.cache.getUser(id));

    // Monadic composition: chain dependent operations
    const profileMonad = await userMonad.flatMap(user =>
      new AsyncMonad(async () => {
        // Simulate profile enrichment
        await new Promise(resolve => setTimeout(resolve, 50));
        return {
          _tag: 'Success' as const,
          value: {
            user,
            profile: `Enhanced profile for ${user.name}`
          }
        };
      })
    );

    return profileMonad.run();
  }
}

// Usage example
async function demonstrateMonadPlus() {
  const orchestrator = new UserServiceOrchestrator(
    new PrimaryUserService(),
    new BackupUserService(),
    new CacheUserService()
  );

  console.log('Testing MonadPlus fallback chain:');

  for (let i = 1; i <= 5; i++) {
    const result = await orchestrator.getUserWithFallback(i);

    if (result._tag === 'Success') {
      console.log(`User ${i}:`, result.value);
    } else {
      console.log(`Failed to get user ${i}:`, result.error);
    }
  }

  console.log('\nTesting monadic composition:');

  const profileResult = await orchestrator.getUserProfile(1);
  if (profileResult._tag === 'Success') {
    console.log('User profile:', profileResult.value);
  } else {
    console.log('Failed to get profile:', profileResult.error);
  }
}

// Run the demonstration
demonstrateMonadPlus().catch(console.error);
