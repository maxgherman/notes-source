-- Choose concrete types for illustration
type A = Int
type B = String
type C = Int

-- Concrete functions
f :: A -> B
f = show

g :: B -> C
g = length

-- Composition example
composed :: A -> C
composed = g . f

-- Hom-set perspective
-- Hom(A, B) contains all functions A -> B
-- Hom(B, C) contains all functions B -> C
-- Composition gives us: Hom(A, B) × Hom(B, C) -> Hom(A, C)
-- This reveals composition as a fundamental operation on hom-sets!
