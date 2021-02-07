#include "stdio.h"

extern int camlMelse__entry();
void caml_curry2() {}

int caml_ml_open_descriptor_out(int fd) {
  // we'll just treat this as stdout for now.
  return 1;
}

char format_string[1024];

int caml_ml_output(int out_channel, char* string, int position, int length) {
  unsigned long int* block = (unsigned long int*)string - 1;
  unsigned long int len = (*block >> 10) * 8;
  fwrite(string, 1, len, stdout);
  return 1;
}

char* caml_format_int(const char* fmt, long int arg) {
  unsigned long int* result = (unsigned long int*)format_string;
  unsigned long int count = sprintf(format_string + 8, "%ld\n", (arg - 1) / 2);
  format_string[count + 8] = 0;
  format_string[count + 9] = 1;
  format_string[count + 10] = 2;
  format_string[count + 11] = 3;
  format_string[count + 12] = 4;
  format_string[count + 13] = 5;
  format_string[count + 14] = 6;
  format_string[count + 15] = 7;

  result[0] = (((count + 8) / 8) << 10) | 252;

  return (format_string + 8);
}

int main() {
  printf("Hello, LLVM!\n");

  camlMelse__entry();

  return 0;
}
