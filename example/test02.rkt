#lang tiny-opalclj
//#eval eval-conc
#eval eval-abs
    
class A {
  int f(int i = 1) {
    int j;
    abs { j = 2; i = i + j; }
    return i;
  }
}
