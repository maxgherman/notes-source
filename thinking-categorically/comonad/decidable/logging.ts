interface Logger<A> {
  log: (value: A) => void
}

type Either<B, C> =
  | { tag: 'left', value: B }
  | { tag: 'right', value: C }

const left = <B, C = never>(value: B): Either<B, C> =>
  ({ tag: 'left', value })

const right = <C, B = never>(value: C): Either<B, C> =>
  ({ tag: 'right', value })

// Decidable instance for Logger
const lose = <A>(impossible: (a: A) => never): Logger<A> => ({
  log: impossible
})

const choose = <A, B, C>(
  discriminate: (a: A) => Either<B, C>,
  loggerB: Logger<B>,
  loggerC: Logger<C>
): Logger<A> => ({
  log: (a: A) => {
    const result = discriminate(a)
    if (result.tag === 'left') {
      loggerB.log(result.value)
    } else {
      loggerC.log(result.value)
    }
  }
})

// Usage: Route different log levels to appropriate handlers
type LogLevel = 'INFO' | 'ERROR'

interface LogEntry {
  level: LogLevel
  message: string
  timestamp: Date
}

const infoLogger: Logger<string> = {
  log: (msg) => console.log(`INFO: ${msg}`)
}

const errorLogger: Logger<string> = {
  log: (msg) => console.error(`ERROR: ${msg}`)
}

const routeLogger: Logger<LogEntry> = choose(
  (entry: LogEntry) => entry.level === 'INFO'
    ? left<string, string>(entry.message)
    : right<string, string>(entry.message),
  infoLogger,
  errorLogger
)

// More sophisticated: Route by content type
type Content = { type: 'success', data: string } | { type: 'error', error: Error }

const successLogger: Logger<string> = {
  log: (data) => console.log(`✓ ${data}`)
}

const failureLogger: Logger<Error> = {
  log: (error) => console.error(`✗ ${error.message}`)
}

const contentLogger: Logger<Content> = choose(
  (content: Content) => content.type === 'success'
    ? left<string, Error>(content.data)
    : right<Error, string>(content.error),
  successLogger,
  failureLogger
)

// Example usage and execution
const testLogEntries: LogEntry[] = [
  { level: 'INFO', message: 'User logged in', timestamp: new Date() },
  { level: 'ERROR', message: 'Database connection failed', timestamp: new Date() },
  { level: 'INFO', message: 'File uploaded successfully', timestamp: new Date() }
]

const testContent: Content[] = [
  { type: 'success', data: 'Payment processed' },
  { type: 'error', error: new Error('Invalid credit card') },
  { type: 'success', data: 'Order confirmed' }
]

console.log('=== Routing Log Entries ===')
testLogEntries.forEach(entry => {
  routeLogger.log(entry)
})

console.log('\n=== Routing Content by Type ===')
testContent.forEach(content => {
  contentLogger.log(content)
})

// Output:
// === Routing Log Entries ===
// INFO: User logged in
// ERROR: Database connection failed
// INFO: File uploaded successfully
//
// === Routing Content by Type ===
// ✓ Payment processed
// ✗ Invalid credit card
// ✓ Order confirmed
