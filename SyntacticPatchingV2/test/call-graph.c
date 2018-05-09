#include <stdio.h>

int foo() {
  int a;
  return 1;
}
int bar() {
  int a;
  foo();
  return 2;
}

int aaa() {
  int a;
  foo();
  int b;
  bar();
  return 3;
}
int bbb() {
  int a;
  foo() + bar();
  int b;
  return 4;
}
