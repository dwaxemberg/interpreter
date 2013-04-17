description:A function call that ignores the return value.\nvalue:2\n\n
var count = 0;

f(a,b) {
    count = count + 1;
    a = a + b;
    return a;
}

main() {
    f(1, 2);
    f(3, 4);
    return count;
}
