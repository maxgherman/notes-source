// Loading states are coproducts - exactly one state at a time
type LoadingState<T> =
  | { type: 'idle' }
  | { type: 'loading'; progress?: number }
  | { type: 'success'; data: T }
  | { type: 'error'; message: string; retryCount: number };

// React component handles all cases
function UserProfile({ userId }: { userId: string }) {
  const [state, setState] = useState<LoadingState<User>>({ type: 'idle' });

  const renderContent = () => {
    switch (state.type) {  // Case analysis
      case 'idle':
        return <button onClick={loadUser}>Load User</button>;
      case 'loading':
        return <Spinner progress={state.progress} />;
      case 'success':
        return <UserCard user={state.data} />;
      case 'error':
        return <ErrorMessage message={state.message} onRetry={loadUser} />;
    }
  };

  return <div>{renderContent()}</div>;
}