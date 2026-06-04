function processUserRequest(userId, requestData) {
  const user = database.getUser(userId);
  if (user === null) {
    return { error: "User not found" };
  }

  const permissions = auth.checkPermissions(user);
  if (permissions === null) {
    return { error: "Unauthorized" };
  }

  const validatedData = validator.validate(requestData);
  if (validatedData === null) {
    return { error: "Invalid data" };
  }

  const result = processor.process(validatedData);
  if (result === null) {
    return { error: "Processing failed" };
  }

  const saved = storage.save(result);
  if (saved === null) {
    return { error: "Storage failed" };
  }

  return { success: saved };
}
