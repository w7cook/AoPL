--BEGIN:A15
var f = function(x) { x * x };
f(10)
--END:A15

# IntV 100

--BEGIN:Prob5
var add = function(a) { function(b) { a + b } };
add(3)(2)
--END:Prob5

# ERROR: Variable a undefined

--BEGIN:Prob7
var n = 2;
var add = function(a) { n+1 };
var n = 10;
add(3)
--END:Prob7

# IntV 3


