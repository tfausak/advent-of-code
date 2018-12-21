-- stack --resolver lts-12.0 script
main = print 986758

-- To solve this one, I took my input and translated it into C. As the problem
-- states, it appears to run forever. So I added some debug output before the
-- conditional that looked like it would cause the program to halt. The value I
-- printed out was the answer, so that was it.

-- It took me quite a while to get to that solution though. I tried for a long
-- time to turn the unstructured assembly/C into something structured. Sadly I
-- don't have any practice doing that, so I kept making mistakes and ending up
-- stuck. Needless to say, I wasn't a fan of this puzzle.

-- For posterity, this is the C program I created from my input:

{-
#include <stdio.h>
int main () {
  int r0 = 986758; // This register is the one that I can change.
  int r1 = 0;
  int r2 = 0;
  int r3 = 0;
  int r4 = 0;
  // This register is the instruction pointer, so I didn't need it once I
  // converted the assembly into C.
  // int r5 = 0;

  r3 = 123;
l0:
  r3 = r3 & 456;
  r3 = r3 == 72;
  if (r3) { goto l1; }
  goto l0;
l1:
  r3 = 0;
l2:
  r1 = r3 | 65536;
  r3 = 9450265;
l3:
  r4 = r1 & 255;
  r3 = r3 + r4;
  r3 = r3 & 16777215;
  r3 = r3 * 65899;
  r3 = r3 & 16777215;
  r4 = 256 > r1;
  if (r4) { goto l4; }
  goto l5;
l4:
  goto l10;
l5:
  r4 = 0;
l6:
  r2 = r4 + 1;
  r2 = r2 * 256;
  r2 = r2 > r1;
  if (r2) { goto l7; }
  goto l8;
l7:
  goto l9;
l8:
  r4 = r4 + 1;
  goto l6;
l9:
  r1 = r4;
  goto l3;
l10:
  r4 = r3 == r0;
  printf("%8d %8d %8d %8d\n", r1, r2, r3, r4);
  if (r4) { return 0; };
  goto l2;
}
-}
