type User = {
    firstName: string;
    lastName: string;
}

// API responses are coproducts - either success OR error
type ApiResponse<T> =
  | { success: true; data: T }
  | { success: false; error: string; code: number };

// Case analysis handles both possibilities
function handleUserResponse(response: ApiResponse<User>): string {
  if (response.success) {
    return `Welcome, ${response.data.firstName}!`;  // Handle success case
  } else {
    return `Error ${response.code}: ${response.error}`; // Handle error case
  }
}

// Network requests naturally produce coproducts
async function fetchUser(id: number): Promise<User | Error> {
  try {
    const response = await fetch(`/api/users/${id}`);
    return await response.json(); // Success case
  } catch (error) {
    return new Error(`Failed to fetch user: ${error}`); // Error case
  }
}