description:A function without a return statement.
value:35

var x = 0;
var y = 0;

setx(a) {
    x = a;
}

sety(b) {
    y = b;
}

main() {
    setx(5);
    sety(7);
    return x * y;
}
