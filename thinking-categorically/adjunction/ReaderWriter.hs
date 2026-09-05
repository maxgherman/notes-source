module Main where

newtype Writer w a = Writer (w, a)
newtype Reader r a = Reader (r -> a)

readerWriterUnit :: a -> Reader r (Writer r a)
readerWriterUnit a = Reader (\r -> Writer (r, a))

readerWriterCounit :: Writer r (Reader r a) -> a
readerWriterCounit (Writer (r, Reader f)) = f r

leftAdjunct :: (Writer r a -> b) -> a -> Reader r b
leftAdjunct f a = Reader (\r -> f (Writer (r, a)))

rightAdjunct :: (a -> Reader r b) -> Writer r a -> b
rightAdjunct f (Writer (r, a)) =
  let Reader g = f a
  in g r

format :: Writer String Int -> String
format (Writer (prefix, n)) = prefix ++ show n

formatted :: Int -> Reader String String
formatted = leftAdjunct format

exampleReaderWriter :: String
exampleReaderWriter =
  let Reader run = formatted 42
  in run "answer: "

main :: IO ()
main = putStrLn exampleReaderWriter
