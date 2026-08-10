interface Theme {
  name: string
  primaryColor: string
  backgroundColor: string
}

interface User {
  id: number
  name: string
  email: string
  role: 'user' | 'admin'
}

interface Permission {
  action: string
  resource: string
}

interface Environment {
  theme: Theme
  user: User
  permissions: readonly Permission[]
}

// Store<S, A> contains a value for every position S and a current position.
interface Store<S, A> {
  position: S
  peek: (position: S) => A
}

const mapStore = <S, A, B>(f: (value: A) => B) =>
  (store: Store<S, A>): Store<S, B> => ({
    position: store.position,
    peek: (position) => f(store.peek(position))
  })

const seek = <S, A>(position: S, store: Store<S, A>): Store<S, A> => ({
  position,
  peek: store.peek
})

const extract = <S, A>(store: Store<S, A>): A =>
  store.peek(store.position)

const duplicate = <S, A>(
  store: Store<S, A>
): Store<S, Store<S, A>> => ({
  position: store.position,
  peek: (position) => ({
    position,
    peek: store.peek
  })
})

const extend = <S, A, B>(f: (store: Store<S, A>) => B) =>
  (store: Store<S, A>): Store<S, B> => ({
    position: store.position,
    peek: (position) => f({
      position,
      peek: store.peek
    })
  })

type Component<A> = Store<Environment, A>

const makeComponent = <A>(
  environment: Environment,
  render: (environment: Environment) => A
): Component<A> => ({
  position: environment,
  peek: render
})

const hasPermission = (
  environment: Environment,
  action: string,
  resource: string
): boolean =>
  environment.permissions.some(
    permission =>
      permission.action === action && permission.resource === resource
  )

const lightEnvironment: Environment = {
  theme: {
    name: 'Light',
    primaryColor: '#333333',
    backgroundColor: '#ffffff'
  },
  user: {
    id: 1,
    name: 'Alice Smith',
    email: 'alice@example.com',
    role: 'user'
  },
  permissions: [
    { action: 'read', resource: 'profile' },
    { action: 'write', resource: 'profile' }
  ]
}

const darkEnvironment: Environment = {
  theme: {
    name: 'Dark',
    primaryColor: '#ffffff',
    backgroundColor: '#333333'
  },
  user: {
    id: 2,
    name: 'Bob Admin',
    email: 'bob@admin.com',
    role: 'admin'
  },
  permissions: [
    { action: 'read', resource: 'profile' },
    { action: 'write', resource: 'profile' },
    { action: 'read', resource: 'users' }
  ]
}

// The renderer works at every environment; lightEnvironment is the focus.
const userProfile: Component<string> = makeComponent(
  lightEnvironment,
  environment => 'Hello, ' + environment.user.name + '!'
)

// extend receives the whole component focused at each environment.
const themedProfile: Component<string> = extend(
  (component: Component<string>) => {
    const environment = component.position
    return '<div style="color: ' + environment.theme.primaryColor +
      '; background: ' + environment.theme.backgroundColor + '">' +
      extract(component) +
      '</div>'
  }
)(userProfile)

const conditionalProfile: Component<string> = extend(
  (component: Component<string>) =>
    hasPermission(component.position, 'read', 'profile')
      ? extract(component)
      : '<div>Access denied</div>'
)(userProfile)

const adminPanel: Component<string> = extend(
  (component: Component<string>) => {
    const environment = component.position
    if (environment.user.role !== 'admin') {
      return extract(component)
    }

    return '<section class="admin-panel"><h2>Admin Dashboard</h2>' +
      extract(component) +
      '<button>Manage Users</button></section>'
  }
)(themedProfile)

const renderAt = <A>(
  environment: Environment,
  component: Component<A>
): A => extract(seek(environment, component))

console.log(renderAt(lightEnvironment, themedProfile))
console.log(renderAt(darkEnvironment, themedProfile))
console.log(renderAt(lightEnvironment, conditionalProfile))
console.log(renderAt(darkEnvironment, adminPanel))

// Check the comonad laws observationally at both environments.
const environments = [lightEnvironment, darkEnvironment]

const extendExtract = extend<Environment, string, string>(extract)(userProfile)
console.log(
  'extend extract = id:',
  environments.every(environment =>
    renderAt(environment, extendExtract) ===
      renderAt(environment, userProfile)
  )
)

const describe = (component: Component<string>): string =>
  component.position.theme.name + ': ' + extract(component)

console.log(
  'extract . extend describe = describe:',
  extract(extend(describe)(userProfile)) === describe(userProfile)
)

const extractedDuplicate = extract(duplicate(userProfile))
console.log(
  'extract . duplicate = id:',
  environments.every(environment =>
    renderAt(environment, extractedDuplicate) ===
      renderAt(environment, userProfile)
  )
)
