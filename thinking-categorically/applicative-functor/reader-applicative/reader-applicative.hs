newtype Reader r a = Reader { runReader :: r -> a }

instance Applicative (Reader r) where
    pure x = Reader (\_ -> x)
    Reader f <*> Reader x = Reader (\r -> f r (x r))
