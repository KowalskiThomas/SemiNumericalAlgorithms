#include <iostream>

#include "code.h"
#include "utils.h"

int main(const int argc, char** argv)
{
  unsigned long long min_limbs = 1;
  unsigned long long max_limbs = 15000;
  unsigned long long step = 10;
  if(argc >= 3)
  {
      min_limbs = strtoull(argv[1], &(argv[1])+strlen(argv[1]), 10);
      max_limbs = strtoull(argv[2], &(argv[2])+strlen(argv[2]), 10);
  }

  test_time(root<2>, min_limbs, max_limbs, step);
  return 0;
}
