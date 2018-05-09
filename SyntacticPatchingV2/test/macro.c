#define FUNC_MACRO(name) int name () {return 8;}

static FUNC_MACRO (gen_macro);
#define VAR 3

int a = VAR;

int main() {
  gen_macro();
  return 0;
}
