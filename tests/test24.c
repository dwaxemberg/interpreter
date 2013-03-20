description: The sort of assignment no sane person would ever do should work
value: 72

var x;
var y;
var z;
var w = (x = 6) + (y = z = 20);
return w + x + y + z;
