#include "helium/type/IOHelper.h"


std::string FileIOHelper::GetIntCode() {
  return R"prefix(
int get_int() {
  int ret;
  fscanf(int_input_fp, "%d", &ret);
  return ret;
}
void input_int(int *a) {
  *a = get_int();
}
void input_int_name(int *a, char *name) {
  *a = get_int();
  fprintf(output_fp, "input: int %s=%d\n", name, *a);
}

void output_int(int *a, char *name) {
  fprintf(output_fp, "output: int %s=%d\n", name, *a);
}

char get_char() {
  char ret;
  fscanf(char_input_fp, "%c", &ret);
  return ret;
}
void input_char(char *c) {
  *c = get_char();
}
void input_char_name(char *c, char *name) {
  *c = get_char();
  fprintf(output_fp, "input: char %s=%c\n", name, *c);
}

void output_char(char *c, char *name) {
  fprintf(output_fp, "output: char %s=%c\n", name, *c);
}


bool get_bool() {
  int tmp;
  fscanf(bool_input_fp, "%d", &tmp);
  return tmp==1;
}
void input_bool(bool *b) {
  *b = get_bool();
}
void input_bool_name(bool *b, char *name) {
  *b = get_bool();
  fprintf(output_fp, "input: bool %s=%d\n", name, *b);
}
void output_bool(bool *b, char *name) {
  fprintf(output_fp, "output: bool %s=%d\n", name, *b);
}


)prefix";
}

std::string FileIOHelper::GetGlobalFilePointers() {
  return R"prefix(
// global file pointers
FILE *int_input_fp;
FILE *char_input_fp;
FILE *bool_input_fp;
FILE *output_fp;
)prefix";
}

/**
 * Insert at the beginning of main.c
 */
std::string FileIOHelper::GetIOCode() {
  return GetGlobalFilePointers() + GetIntCode();
}

/**
 * input at the beginning of main
 */
std::string FileIOHelper::GetFilePointersInit() {
  return R"prefix(
  int_input_fp = fopen("/home/hebi/.helium.d/input_values/int.txt", "r");
  char_input_fp = fopen("/home/hebi/.helium.d/input_values/char.txt", "r");
  bool_input_fp = fopen("/home/hebi/.helium.d/input_values/bool.txt", "r");
  // this file will not only record the output, but also the input
  output_fp = fopen("helium_output.txt", "w");
)prefix";
}
