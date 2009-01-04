#include <stdio.h>

#include "EvalHaskell.h"

int main(int argc, char *argv[])
{
  char *p;
  hs_init(&argc, &argv);
  p = hs_eval_s("show $ let fibs = 1:1:zipWith (+) fibs (tail fibs) in fibs !! 20");
  if (p == NULL)
  	  printf("failed!\n");
  else
	  printf("%s\n",p);
  hs_exit();
  return 0;
}
