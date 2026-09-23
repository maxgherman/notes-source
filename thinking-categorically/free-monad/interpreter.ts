// Free Monad implementation using interfaces
interface Free<F, A> {
  readonly tag: 'Pure' | 'FreeBind';
  fold<B>(
    pure: (a: A) => B,
    free: (fa: F) => B
  ): B;
}

interface Pure<F, A> extends Free<F, A> {
  readonly tag: 'Pure';
  readonly value: A;
}

interface FreeBind<F, A> extends Free<F, A> {
  readonly tag: 'FreeBind';
  readonly fa: F;
}

// Constructors for Free Monad
function makePure<F, A>(value: A): Pure<F, A> {
  return {
    tag: 'Pure',
    value,
    fold<B>(pure: (a: A) => B, free: (fa: F) => B): B {
      return pure(this.value);
    }
  };
}

function makeFreeBind<F, A>(fa: F): FreeBind<F, A> {
  return {
    tag: 'FreeBind',
    fa,
    fold<B>(pure: (a: A) => B, free: (fa: F) => B): B {
      return free(this.fa);
    }
  };
}

// Console operations using interfaces
interface ConsoleF<A> {
  readonly tag: 'WriteLine' | 'ReadLine';
  map<B>(f: (a: A) => B): ConsoleF<B>;
}

interface WriteLine<A> extends ConsoleF<A> {
  readonly tag: 'WriteLine';
  readonly message: string;
  readonly next: A;
}

interface ReadLine<A> extends ConsoleF<A> {
  readonly tag: 'ReadLine';
  readonly continuation: (input: string) => A;
}

// Constructors for console operations
function makeWriteLine<A>(message: string, next: A): WriteLine<A> {
  return {
    tag: 'WriteLine',
    message,
    next,
    map<B>(f: (a: A) => B): WriteLine<B> {
      return makeWriteLine(this.message, f(this.next));
    }
  };
}

function makeReadLine<A>(continuation: (input: string) => A): ReadLine<A> {
  return {
    tag: 'ReadLine',
    continuation,
    map<B>(f: (a: A) => B): ReadLine<B> {
      return makeReadLine((input) => f(this.continuation(input)));
    }
  };
}

// Type alias for our console Free Monad
type ConsoleProgram<A> = Free<ConsoleF<ConsoleProgram<A>>, A>;

// Smart constructors
function writeLine(message: string): ConsoleProgram<void> {
  return makeFreeBind(makeWriteLine(message, makePure(undefined)));
}

function readLine(): ConsoleProgram<string> {
  return makeFreeBind(makeReadLine((input) => makePure(input)));
}

function pure<A>(value: A): ConsoleProgram<A> {
  return makePure(value);
}

// Monadic bind operation
function bind<A, B>(ma: ConsoleProgram<A>, f: (a: A) => ConsoleProgram<B>): ConsoleProgram<B> {
  return ma.fold(
    (a) => f(a),
    (fa) => makeFreeBind(fa.map((nextProg: ConsoleProgram<A>) => bind(nextProg, f)))
  );
}

// Example programs
function greetingProgram(): ConsoleProgram<void> {
  return bind(writeLine("Hello! What's your name?"), () =>
    bind(readLine(), (name) =>
      bind(writeLine(`Nice to meet you, ${name}!`), () =>
        bind(writeLine("What's your favorite color?"), () =>
          bind(readLine(), (color) =>
            writeLine(`${name} likes ${color}. Great choice!`)
          )
        )
      )
    )
  );
}

function surveyProgram(): ConsoleProgram<void> {
  return bind(writeLine("Welcome to our survey!"), () =>
    bind(writeLine("Are you over 18? (yes/no)"), () =>
      bind(readLine(), (age) =>
        age === "yes"
          ? bind(writeLine("What's your occupation?"), () =>
              bind(readLine(), (job) =>
                writeLine(`Thank you! We have: occupation = ${job}`)
              )
            )
          : writeLine("Thanks for your interest, but this survey is for adults only.")
      )
    )
  );
}

// Real console interpreter (Node.js)
async function runConsoleIO<A>(program: ConsoleProgram<A>): Promise<A> {
  const readline = require('readline');
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  function interpret<T>(prog: ConsoleProgram<T>): Promise<T> {
    return prog.fold(
      async (value) => value,
      async (fa) => {
        if (fa.tag === 'WriteLine') {
          const writeOp = fa as WriteLine<ConsoleProgram<T>>;
          console.log(writeOp.message);
          return interpret(writeOp.next);
        } else if (fa.tag === 'ReadLine') {
          const readOp = fa as ReadLine<ConsoleProgram<T>>;
          return new Promise((resolve) => {
            rl.question('> ', (input: string) => {
              resolve(interpret(readOp.continuation(input)));
            });
          });
        }
        throw new Error('Unknown operation');
      }
    );
  }

  const result = await interpret(program);
  rl.close();
  return result;
}

// Test interpreter (pure)
interface TestResult<A> {
  result: A;
  outputs: string[];
  inputsUsed: string[];
}

function runConsoleTest<A>(
  inputs: string[],
  program: ConsoleProgram<A>
): TestResult<A> {
  let inputIndex = 0;
  const outputs: string[] = [];
  const inputsUsed: string[] = [];

  function interpret<T>(prog: ConsoleProgram<T>): T {
    return prog.fold(
      (value) => value,
      (fa) => {
        if (fa.tag === 'WriteLine') {
          const writeOp = fa as WriteLine<ConsoleProgram<T>>;
          outputs.push(writeOp.message);
          return interpret(writeOp.next);
        } else if (fa.tag === 'ReadLine') {
          const readOp = fa as ReadLine<ConsoleProgram<T>>;
          if (inputIndex >= inputs.length) {
            throw new Error('No more test inputs available!');
          }
          const input = inputs[inputIndex++];
          inputsUsed.push(input);
          return interpret(readOp.continuation(input));
        }
        throw new Error('Unknown operation');
      }
    );
  }

  const result = interpret(program);
  return { result, outputs, inputsUsed };
}

// Mock interpreter
interface MockResult {
  outputs: string[];
  inputsUsed: string[];
}

function runConsoleMock<A>(
  inputs: string[],
  program: ConsoleProgram<A>
): MockResult {
  let inputIndex = 0;
  const outputs: string[] = [];
  const inputsUsed: string[] = [];

  function interpret<T>(prog: ConsoleProgram<T>): void {
    prog.fold(
      () => undefined,
      (fa) => {
        if (fa.tag === 'WriteLine') {
          const writeOp = fa as WriteLine<ConsoleProgram<T>>;
          outputs.push(writeOp.message);
          interpret(writeOp.next);
        } else if (fa.tag === 'ReadLine') {
          const readOp = fa as ReadLine<ConsoleProgram<T>>;
          if (inputIndex >= inputs.length) {
            throw new Error('No mock input available!');
          }
          const input = inputs[inputIndex++];
          inputsUsed.push(input);
          interpret(readOp.continuation(input));
        }
      }
    );
  }

  interpret(program);
  return { outputs, inputsUsed };
}

// Usage example
async function main() {
  console.log("=== Running with real IO ===");
  await runConsoleIO(greetingProgram());

  console.log("\n=== Running with test data ===");
  const testInputs = ["Alice", "blue"];
  const testResult = runConsoleTest(testInputs, greetingProgram());
  console.log("Outputs:", testResult.outputs);
  console.log("Inputs used:", testResult.inputsUsed);

  console.log("\n=== Running with mock ===");
  const mockInputs = ["Bob", "red"];
  const mockResult = runConsoleMock(mockInputs, greetingProgram());
  console.log("Mock result:", mockResult);
}

main().catch(console.error);
