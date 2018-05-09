#include <stdio.h>
#include <stddef.h>

int main() {
  return 0;
}

enum E {
  AAA,
  BBB,
  CCC
};

struct X {
  int a;
  char b;
};

void bar(int a, char b[]) {}

void foo() {
  int a,b,c;
  char d;
  struct X x;
  char buf[5];
  // refvar, binary
  c=a+b;
  // TODO ref func
  // ref-enum-const
  a=AAA;
  // unary
  ++a;
  // call
  bar(a, buf);
  // undefined library function
  strcpy(buf, buf);
  // cast
  a=(int)d;
  // paren
  (a+b);
  // member
  x.a;
  (&x)->a;
  // tenary
  a=b>c?c:b;
  // liter
  a=1;
  // array
  buf[2]='c';
  // TODO init-list
  // TODO vaarg
  // sizeof
  sizeof(a);
  sizeof(int);
  // TODO alignof
  // TODO stmt
  // offsetof
  {
    struct bts_buffer {
      int a;
      char b;
      int c;
      char buf[8];
    };
    struct S {
      char c;
      double d;
    };
    struct bts_buffer *buf;
    int nbuf=8;
    offsetof(struct S, c);
    offsetof(struct S, d);
    offsetof(struct bts_buffer, buf[nbuf]);
  }
  // TODO comp-liter
}
