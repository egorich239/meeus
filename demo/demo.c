#include <stdio.h>

#include "../include/date.h"

int main() {
  struct jd d;
  int err = make_jd_gregorian(2024, 2, 2.0, &d);
  printf("err = %d jd = %f\n", err, d.rep);
}