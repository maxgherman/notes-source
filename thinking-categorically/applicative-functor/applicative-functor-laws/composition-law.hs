-- Define three lists
u = [(+1)]           :: [Int -> Int]
v = [(*2)]           :: [Int -> Int]
w = [10]             :: [Int]

-- Left side: pure (.) <*> u <*> v <*> w
left = pure (.) <*> u <*> v <*> w    -- [((+1) . (*2)) 10] = [21]

-- Right side: u <*> (v <*> w)
right = u <*> (v <*> w)              -- [(+1) ((*2) 10)] = [21]

-- Test for equality
test = left == right                 -- True
