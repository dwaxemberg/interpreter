description: 3 extends
value: Error(B), 4321(C)

class A {
  static var a = 1;
  static var b = 20;
}

class B extends A {
  static var c = 300;

  static main() {
    return a + b + c + d;
  }
}

class C extends B {
  static var d = 4000;

  static main() {
    return a + b + c + d;
  }
}
