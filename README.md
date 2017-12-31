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

## Statistics

| Day | Puzzle                                  | Lines | SLOC | SWOC | Interpreted | Compiled |
|---- | --------------------------------------- | ----- | ---- | ---- | ----------- | ---------|
|   1 | Inverse Captcha                         |     8 |    6 | 57.2 |       0.648 |    0.178 |
|   2 | Corruption Checksum                     |     9 |    6 | 41.7 |       0.650 |    0.197 |
|   3 | Spiral Memory                           |    25 |   19 | 37.2 |       0.700 |    0.174 |
|   4 | High-Entropy Passphrases                |     5 |    3 | 49.3 |       0.650 |    0.206 |
|   5 | A Maze of Twisty Trampolines, All Alike |    20 |   13 | 32.4 |     140.994 |    1.113 |
|   6 | Memory Reallocation                     |    14 |   10 | 45.6 |       0.944 |    0.215 |
|   7 | Recursive Circus                        |    43 |   29 | 40.7 |       1.011 |    0.226 |
|   8 | I Heard You Like Registers              |    28 |   19 | 37.6 |       0.933 |    0.201 |
|   9 | Stream Processing                       |    10 |    8 | 49.0 |       0.701 |    0.228 |
|  10 | Knot Hash                               |    22 |   14 | 46.7 |       1.122 |    0.260 |
|  11 | Hed Ex                                  |    14 |   11 | 34.0 |       0.843 |    0.326 |
|  12 | Digital Plumber                         |    11 |    5 | 63.6 |       0.669 |    0.237 |
|  13 | Packet Scanners                         |     7 |    6 | 54.3 |      29.483 |    0.548 |
|  14 | Disk Defragmentation                    |    32 |   21 | 49.9 |      11.210 |    2.483 |
|  15 | Dueling Generators                      |    12 |   10 | 57.0 |     142.395 |   16.423 |
|  16 | Permutation Promenade                   |    36 |   25 | 42.0 |       1.236 |    0.381 |
|  17 | Spinlock                                |    12 |    8 | 39.8 |     150.862 |    1.822 |
|  18 | Duet                                    |   87¹ |   65 | 36.0 |      7.360² |   0.236² |
|  19 | A Series of Tubes                       |    14 |   12 | 47.6 |       0.820 |    0.262 |
|  20 | Particle Swarm                          |    93 |   49 | 46.5 |      23.887 |    0.956 |
|  21 | Fractal Art                             |    55 |   46 | 43.1 |      44.030 |   10.999 |
|  22 | Sporifica Virus                         |    36 |   26 | 43.9 |       9.147 |    8.553 |
|  23 | Coprocessor Conflagration               |    30 |   20 | 41.5 |       1.345 |    0.233 |
|  24 | Electromagnetic Moat                    |    34 |   21 | 38.3 |      24.172 |    2.014 |
|  25 | The Halting Problem                     |   59¹ |   48 | 45.4 |             |          |
|     | • hardcoded                             |       |      |      |        40.8 |    0.965 |
|     | • generic                               |       |      |      |        44.4 |    2.062 |

[1] The code contains multiple variants

[2] Those timings perform part one twice, with a different implementation.
