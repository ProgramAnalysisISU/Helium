#include <stdio.h>


void foo(int argc, char *argv[]) {
  // decl stmt
  int a,b,c;
  char d;
  int e[5];
  char *s="hello";
  printf("\"");
  //  if
  // comp
  if (a>0) {
    a=8+b + argc;
  } else {
    // expr
    a=9;
  }
  // for
  for (int i=0;i<a;i++) {
    a++;
    b++;
  }
  // while
  while(a>0) {
    e[a]=a;
    e[a]=b;
    // continue
    continue;
  }
  // do
  do {
    // label
  lab:
    e[a]=a;
    e[a]=b;
  } while (a>0);
  // switch
  switch (a) {
    // case
    // goto
  case 1: a=2; goto lab;
    // break
  case 2: a=3; a=4; break;
  case 3: {a=5;}
    // default
  default: a=6;
  }
  // empty stmt
  if (a>0) ;
  // return
  return;
}


int main() {
  // return + value
  return 0;
}

int bar() {
  int a;
  int b;
  a+b;
  return a;
}
