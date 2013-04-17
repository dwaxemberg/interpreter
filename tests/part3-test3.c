description:A function that changes global variables.
value:45

var x = 1;
var y = 10;
var r = 0;

main() {
    while (x < y) {
        r = r + x;
        x = x + 1;
    }
    return r;
}
