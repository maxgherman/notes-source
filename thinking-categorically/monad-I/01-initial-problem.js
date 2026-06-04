const database = {
    getUser: (userId) => {
        // Simulate a user fetch
        return { id: userId, name: "John Doe" };
    }
};

const auth = {
    checkPermissions: (user) => {
        // Simulate permission check
        return user ? { canAccess: true } : null;
    }
};

const validator = {
    validate: (data) => {
        // Simulate data validation
        return data && typeof data === 'object' ? data : null;
    }
};

const processor = {
    process: (data) => {
        // Simulate data processing
        return data ? { processed: true, data } : null;
    }
};

const storage = {
    save: (data) => {
        // Simulate data storage
        return data ? { saved: true, data } : null;
    }
};

function processUserRequest(userId, requestData) {
    const user = database.getUser(userId);

    if (user) {
        const permissions = auth.checkPermissions(user);
        if (permissions) {
            const validatedData = validator.validate(requestData);

            if (validatedData) {
                const processedData = processor.process(validatedData);

                if (processedData) {
                    const saved = storage.save(processedData);

                    if(saved) {
                        return { success: saved };
                    }
                    else {
                        return { error: "Storage failed" };
                    }
                } else {
                    return { error: "Processing failed" };
                }
            }
            else {
                return { error: "Invalid data" };
            }
        }
        else{
            return { error: "Permission denied" };
        }
    }
    else{
        return { error: "User not found" };
    }
}

var result = processUserRequest(1, { key: "value" });

console.log(result);
