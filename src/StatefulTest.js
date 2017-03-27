var x = mutable 3;
var y = mutable true;
if (@y) { x = @x + 1 } else { x };
@x

# IntV 4

var x = mutable 3;
var y = mutable 7;
x = @x + @y;
@x * @y

# IntV 70
