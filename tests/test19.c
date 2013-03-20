description: Multiple assignments should work
value: 30

var x;
var y;
var z = x = y = 10;
return x + y + z;
