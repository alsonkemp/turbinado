#include <stdio.h>

#include "EvalHaskell.h"

int main(int argc, char *argv[])
{
  int *p;
  hs_init(&argc, &argv);
  p = hs_eval_i("let fibs = 1:1:zipWith (+) fibs (tail fibs) in fibs !! 20 :: Int");
  if (p == NULL)
	  printf("failed!\n");
  else
	  printf("%d\n",*p);
  hs_exit();
  return 0;
}
