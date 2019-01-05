# Advent of Code

These are my solutions to the [Advent of Code][]. There is a top-level folder
for each year. Inside each of those is a folder for each day. I write my
solutions in Haskell, and I recommend that you use [Stack][] to run them. For
example:

``` sh
$ cd 2017/1
$ stack 1.hs
1031
```

When working on solutions, I typically use [ghcid][] to get fast feedback. For
instance:

``` sh
$ cd 2017/1
$ stack --resolver lts-13.0 exec --package ghcid -- \
  ghcid --test main 1.hs
...
```

These solutions aren't meant to show best practices. They're quick and dirty.
They are probably too clever for their own good.

You can find other solutions, both in Haskell and other languages, on this list:
<https://github.com/Bogdanp/awesome-advent-of-code>.

[Advent of Code]: https://adventofcode.com
[Stack]: https://docs.haskellstack.org/en/stable/README/
[ghcid]: https://github.com/ndmitchell/ghcid
