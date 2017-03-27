
var x = mutable 3;
var y = mutable true;
if (@y) { x = @x + 1 } else { x };
@x

# IntV 4

var x = mutable 3;
var y = mutable 7;
x = @x + @y;
y = @y * @x

# IntV 70

var x = mutable 3; 
var f = function(p) { p = @p+1 };
f(x);
@x

# IntV 4
