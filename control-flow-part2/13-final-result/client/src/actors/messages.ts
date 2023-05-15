export type MessageBase = {
    type: string
}

export type ErrorMessage = MessageBase & {
    data: {
        field?: string
        error: Error
    }[]
}

export type ValidateMessage = MessageBase & {
    data: {
      x: string
      y: string
    }
}

export type WorkerMessage = MessageBase & {
    data: {
        x: number
        y: number
    }
}

export type EnvironmentMessage = MessageBase & {
    data: {
        serverURL: string
    }
}

export type EnvironmentRequestMessage = MessageBase & {
    receive: (message: EnvironmentMessage) => Promise<void>
}

export type StartMessage = MessageBase & {
    data: {
        x: string
        y: string
    }
}

export type ResultMessage = MessageBase & {
    data: number
}

export type RunMessage =  StartMessage | ResultMessage