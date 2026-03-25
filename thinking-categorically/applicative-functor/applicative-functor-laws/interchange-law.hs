-- Define list of functions
u = [(+1), (*2)]

-- A pure value
y = 3

-- Left side of the law
left = u <*> pure y    -- [4,6]

-- Right side of the law
right = pure (\f -> f y) <*> u  -- [4,6]

-- Check equality
test = left == right  -- True
