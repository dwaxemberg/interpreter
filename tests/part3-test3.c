description:A function that changes global variables.\nvalue:45\n\n
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
