description: Really strange assignments should work
value: 12

var x = 10;
x = (x = 6) + x;
return x;
