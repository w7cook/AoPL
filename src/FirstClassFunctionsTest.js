var T = function(a) { function(b) { a } };
var F = function(a) { function(b) { b } };
var not = function(b) { b(F)(T) };
not(F)

# ClosureV "a" (Function "b" (Variable "a")) []

var x = 5;
var f = function(y) { x - y };
var x = f(9);
f(x)

# IntV 9

var x = 5;
var f = function(y) { 
		var y = x * y; 
		function(x) { 
			x + y 
		} 
	};
var g = f(2);
g(5)

# IntV 15

var comp = function(f) { 
		function(g) { 
			function(x) { f(g(x)) }
		}};
var inc = function(x) { x + 1 };
var square = function(x) { x * x };
var f = comp(inc)(square);
f(5)

# IntV 26

var map = function(f) { 
		function(x) { 
			function(y) { 
				f(x) + f(y) 
			}
		}
	};
var g = function(x) { x + 1 };
map(g)(3)(4)

# IntV 9
