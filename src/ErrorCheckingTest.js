
--BEGIN:Hand28
x

# Error "Variable x undefined"

--END:Hand28

--BEGIN:Hand32

3 / 0

# Error "Divide by zero"
--END:Hand32

--BEGIN:Hand34
-true

# Error "Unary Neg called with invalid argument BoolV True"

!4

# Error "Unary Not called with invalid argument IntV 4"

4 + true

# Error "Binary Add called with invalid arguments IntV 4, BoolV True"

4 && false

# Error "Binary And called with invalid arguments IntV 4, BoolV False"
--END:Hand34
