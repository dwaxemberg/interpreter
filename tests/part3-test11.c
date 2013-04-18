description:Call-by-reference test.
value:3421

swap1(x, y) {
    var temp = x;
    x = y;
    y = temp;
}

swap2(&x, &y) {
    var temp = x;
    x = y;
    y = temp;
}

main() {
    var a = 1;
    var b = 2;
    swap1(a,b);
    var c = 3;
    var d = 4;
    swap2(c,d);
    return a + 10*b + 100*c + 1000*d;
}