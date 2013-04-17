description:Test a main function.\nvalue:10\n\n
main() {
    var x = 10;
    var y = 20;
    var z = 30;
    var min = 0;

    if (x < y)
        min = x;
    else
        min = y;
    if (min > z)
        min = z;
    return min;
}
