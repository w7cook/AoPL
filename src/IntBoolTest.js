--BEGIN:More19

4

# IntV 4

-4 - 6

# IntV (-10)

if (3==6) -2 else -7

# IntV (-7)

3*(8 + 5)

# IntV 39

3 + 8 * 2

# IntV 19
--END:More19

--BEGIN:More98
if (3 > 3*(8 + 5)) 1 else 0

# IntV 0

2 + (if (3 <= 0) 9 else -5)

# IntV (-3)

--END:More98

--BEGIN:Type6
if (true) 5 else 8

# IntV 5

3 + true

# Error: Value.hs:(19,1)-(29,47): Non-exhaustive patterns in function binary

3 || true

# Error: Value.hs:(19,1)-(29,47): Non-exhaustive patterns in function binary

-true

# Error: Value.hs:(16,1)-(17,31): Non-exhaustive patterns in function unary

--END:Type6

