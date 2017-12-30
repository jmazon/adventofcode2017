# adventofcode2017
Looks like it's a habit now.

So this year ends up pure Haskell, because why not.  All in all, it
wasn't so bad.  Reasonably compact code, sometimes a bit tedious on
the parsing (in my experience, AoC tends to be well-suited to regex
parsing, to which Haskell tends to be resistant), a single
lazyness-caused space leak crazyness over the entire 25 days.

The code has been tidied up a bit compared to what got me to pass.
And updated to solve both parts in a single run.

The three out-of-order commits are:

- Day 13: wasn't accessible to the node from which I initially pushed
  to github.

- Day 21: I did solve this one last.  Not enough time to solve it on
  the relevant day; I only thought about it to mentally solve it for
  N=small and N=large.  Later during the day, I got spoiled that part
  2 only had a medium-small N, so no need for the better solution.
  That demotivated me enough to keep me from implementing until I
  really needed it.

- Day 20: worst statement/tests impedance mismatch of the season,
  IMHO.  Solved it, then thought "hey, this shouldn't work in the
  general case".  Yet it did.  With no pressing need, I was too
  frustrated with my code to repair it, or clean it up.  So I rewrote
  it last.

A fine experience as always.  Thanks to @topaz and the rest of the AoC
crew!
