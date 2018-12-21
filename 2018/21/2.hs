-- stack --resolver lts-12.0 script
import qualified Data.IntSet as Set
import qualified Data.IORef as IORef
import qualified System.Exit as Exit
import qualified System.IO as IO

-- This program just keeps track of numbers that it's seen and watches for
-- repeats. As soon as it sees a repeat, it returns the previous number it saw.
-- It takes as input the output of the C program embedded below. It's
-- definitely not a nice solution, but it is *a* solution!

main = do
  seenRef <- IORef.newIORef Set.empty
  lastRef <- IORef.newIORef Nothing
  contents <- getContents
  flip mapM_ (map read $ lines contents) $ \ number -> do
    seen <- Set.member number <$> IORef.readIORef seenRef
    if seen
      then do
        print =<< IORef.readIORef lastRef
        Exit.exitSuccess
      else do
        IORef.modifyIORef seenRef $ Set.insert number
        IORef.writeIORef lastRef $ Just number

{-
#include <stdio.h>
int main () {
  int r0 = 0; // This register is the one that I can change.
  int r1 = 0;
  int r2 = 0;
  int r3 = 0;
  int r4 = 0;
  // This register is the instruction pointer, so I didn't need it
  // once I converted the assembly into C.
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
  printf("%d\n", r3);
  if (r4) { return 0; };
  goto l2;
}
-}
