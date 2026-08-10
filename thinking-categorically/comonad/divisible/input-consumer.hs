import Data.Functor.Contravariant (Contravariant (contramap))

-- Contravariant equivalent of Applicative: split an input between two
-- consumers, or accept an input without doing anything.
class Contravariant f => Divisible f where
  conquer :: f a
  divide :: (a -> (b, c)) -> f b -> f c -> f a

-- A side-effecting operation that consumes an input contravariantly.
newtype Consumer a = Consumer
  { runConsumer :: a -> IO ()
  }

instance Contravariant Consumer where
  contramap f (Consumer consume) = Consumer (consume . f)

instance Divisible Consumer where
  conquer = Consumer (\_ -> pure ())

  divide split (Consumer consumeB) (Consumer consumeC) =
    Consumer $ \a -> do
      let (b, c) = split a
      consumeB b
      consumeC c

data Customer = Customer
  { customerEmail :: String
  } deriving Show

data Order = Order
  { orderNumber :: Int
  , orderCustomer :: Customer
  } deriving Show

sendEmail :: String -> String -> IO ()
sendEmail address subject =
  putStrLn $ "Email to " ++ address ++ ": " ++ subject

logOrder :: Consumer Order
logOrder = Consumer $ \order ->
  putStrLn $ "Processing order #" ++ show (orderNumber order)

emailAddress :: Consumer String
emailAddress = Consumer $ \address ->
  sendEmail address "Order confirmed"

-- Adapt an email-address consumer to consume a complete Customer.
emailCustomer :: Consumer Customer
emailCustomer = contramap customerEmail emailAddress

-- Usage: Process complex data by splitting responsibilities
processOrder :: Consumer Order
processOrder = divide orderSplit logOrder emailCustomer
  where
    orderSplit order = (order, orderCustomer order)

ignoreOrder :: Consumer Order
ignoreOrder = conquer

-- Test cases
main :: IO ()
main = do
  let customer = Customer "customer@example.com"
      order = Order 12345 customer

  putStrLn "Input values:"
  print customer
  print order

  putStrLn "\nRun a consumer adapted with contramap:"
  runConsumer emailCustomer customer

  putStrLn "\nRun the combined consumer built with divide:"
  runConsumer processOrder order

  putStrLn "\nRun the no-op consumer built with conquer:"
  runConsumer ignoreOrder order
  putStrLn "No action was performed."
