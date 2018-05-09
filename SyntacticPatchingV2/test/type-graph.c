#include <stdio.h>


enum E {
  aa,
  bb
};

struct A {
  enum E e;
};

typedef struct B {
  struct A *a;
} *Bptr;

struct C {
  struct A a;
  Bptr b;
};

typedef struct C *C;
