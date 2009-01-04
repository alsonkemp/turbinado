#include <stdio.h>

#include "EvalHaskell.h"

int main(int argc, char *argv[])
{
  int *p;
  hs_init(&argc, &argv);
  p = hs_eval_i("show $ case 1 + 2 in{-wrong-} x -> x");
  if (p == NULL)
	  printf("failed!\n");
  else
	  printf("%d\n",*p);
  hs_exit();
  return 0;
}
