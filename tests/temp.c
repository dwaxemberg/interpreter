var x = 10;
var y = 15;

swap(&a, &b) {
    var temp = a;
    a = b;
    b = temp;
}

main() {
    swap(x, y);
}
