description: Crazy assignments should work.
value: 1106

var x;
var y = (x = 5) + (x = 6);
return y * 100 + x;
