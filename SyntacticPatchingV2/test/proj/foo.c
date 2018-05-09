#include <stdio.h>
#include <stdarg.h>

#include "foo.h"

struct FooS1 {
  int a;
  int b;
};

typedef struct FooS2 {
  int a;
  int b;
} FooS2T;


struct match_attr {
  union {
    int pat;
    int *attr;
  } u;
  struct FooS1 x;
  char is_macro;
  unsigned num_attr;
};

struct YETX {
  enum JJJ {
    XXX,YYY
  } a,b;
  enum  {
    ZZZ
  } c,d;
};

enum {
  JJJ
} x,y;

static struct {
  const char *name;
  int *preference;
} advice_config[] = {
};

struct {
  const char *name;
  int *preference;
} advice_config2[] = {
};


// the typedef seems to be named
typedef enum { FIELD_STR, FIELD_ULONG, FIELD_TIME } cmp_type;
typedef enum { COMPARE_EQUAL, COMPARE_UNEQUAL, COMPARE_NONE } cmp_status;
typedef struct {int a;} ts;
typedef struct {int a;} *tsp;



int foo_f1(int a, char *b) {
  return a;
}

int foo_f2(FooS2T s) {
  return foo_f1(s.a, NULL);
}

void foo_f3(int a, const char *one, ...) {
  va_list params;
  int cnt;
  const char *param;
  va_start(params, one);
  for (cnt = 1; (param = va_arg(params, const char *)) != NULL; cnt++)
    ;
  va_end(params);
}

int foo() {
  FooS2T s;
  s.a = 1;
  goto lab;
  foo_f2(s);
 lab:
  printf("helo\nworld");
  return 0;
}
