#include <stdio.h>

void foo() {
  {
    // array type
    int a[8];
  }
  {
    // builtin type
    int a;
    char b;
  }
  {
    // function
    typedef void (*foo)(int a, char b);
    typedef void (*bar)(void);
  }
  // TODO paren
  {
    // pointer
    int *p;
    // TODO C does NOT have pass-by-reference ref, C++ does
    /* int a=9; */
    /* int& ref = &a; */
  }
  {
    // typeof
    typeof(int) a;
    typeof(a) b;
  }
  {
    // tag
    enum A {aa, bb};
    struct B {int a;};
    union C {int a; char b;};
    // tag + variable
    enum D {cc} a;
    // tag + 2 variable
    enum E {dd} b,*c;
    // anonymous tag + variable
    enum {ee} d,*e;
  }

  {
    // typedef
    typedef enum A {aa} B;
    // typedef + anonymous tag
    typedef enum {bb} C;
    // typedef + multiple name
    typedef struct D {} E,*F;
    // identity
    //
    // C++ will not allow this, it will report duplicate definition of
    // A.  That also means in C++ you can just use the tagname without
    // the tag keyword
    typedef enum A *A;
  }
}

void types () {
  int a;
  char *b;
  const int c;
  const int * const d;
  static int e;
  int * restrict f;
  volatile int g;
  typedef int *intptr;
  const intptr *h;
  int i[8];
  char j[] = "hello";
  int (*fp)() = foo;
}

int main() {return 0;}
