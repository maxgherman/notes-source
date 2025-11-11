// Message types form a coproduct - exactly one message type
type Message =
  | { type: 'user_registered'; userId: string; email: string }
  | { type: 'order_created'; orderId: string; amount: number; items: Item[] }
  | { type: 'payment_processed'; orderId: string; transactionId: string }
  | { type: 'notification_sent'; userId: string; channel: string };

// Message handlers use case analysis
class MessageProcessor {
  async handleMessage(message: Message): Promise<void> {
    switch (message.type) {
      case 'user_registered':
        await this.sendWelcomeEmail(message.email);
        await this.createUserProfile(message.userId);
        break;

      case 'order_created':
        await this.validateInventory(message.items);
        await this.processPayment(message.orderId, message.amount);
        break;

      case 'payment_processed':
        await this.fulfillOrder(message.orderId);
        await this.sendConfirmation(message.transactionId);
        break;

      case 'notification_sent':
        await this.logDelivery(message.userId, message.channel);
        break;
    }
  }
}