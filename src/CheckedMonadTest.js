var T = function(a) { function(b) { a } };
var F = function(a) { function(b) { b } };
var not = function(b) { b(F)(T) };
not(F)

# Good (ClosureV "a" (Function "b" (Variable "a")) [])

var x = 5;
var f = function(y) { x - y };
var x = f(9);
f(x)

# Good (IntV 9)

var x = 5;
var f = function(y) { 
		var y = x * y; 
		function(x) { 
			x + y 
		} 
	};
var g = f(2);
g(5)

# Good (IntV 15)

var comp = function(f) { 
		function(g) { 
			function(x) { f(g(x)) }
		}};
var inc = function(x) { x + 1 };
var square = function(x) { x * x };
var f = comp(inc)(square);
f(5)

# Good (IntV 26)

var map = function(f) { 
		function(x) { 
			function(y) { 
				f(x) + f(y) 
			}
		}
	};
var g = function(x) { x + 1 };
map(g)(3)(4)

# Good (IntV 9)

x

# Error "Variable x undefined"

3(5)

# Error "Expected function but found IntV 3"

5 / 0

# Error "Divide by zero"

if (5) 3 else 2

# Error "Expected boolean but found IntV 5"

5 * true

# Error "Binary Mul called with invalid arguments IntV 5, BoolV True"

! 5

# Error "Unary Not called with invalid argument IntV 5"

-true

# Error "Unary Neg called with invalid argument BoolV True"




