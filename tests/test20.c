description: Multiple assignment inside boolean should work
value: 11

var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;
