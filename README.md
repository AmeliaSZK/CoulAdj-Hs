# CoulAdj-Hs
Computes the list of adjacent colours for each colour in an image.

Primary objective is to help me learn Haskell. 

This project declares conformity to [PVP](https://pvp.haskell.org/).

**This project is still in development and is not ready for public consumption.**

"Colour" and "color" will be used interchangeably and arbitrarily in both the code
and documentation.

## Licenses
The license for the files under the `./tests/` directory is specified in `./tests/LICENSE`.

# Educational Objectives
*   Haskell (obviously)
*   Cabal
*   CLI interface in Haskell?

# About this Github repository

*   I made this Github repository public so I could share with my twitter friends.
*   It may be made private in the future.

# Known Requirements
*   GHC
*   Cabal
*   Python 3 (Optional, only to run the benchmark script)

On Windows, we installed GHC and Cabal with [GHCup](https://www.haskell.org/ghcup/)

# How to run
All commands are executed from the root of the repository, which is the directory where this
Readme file is stored.

### For development, copy-pastable:
```
cabal run CoulAdj-Hs ./tests/sample-size-1.png ./tests/results/result-size-1.tsv
```

To check that you have correct results from the above command,
compare `./tests/results/result-size-1.tsv`
with `./tests/golden.tsv` and make sure that both files have the same content.
(It doesn't matter if the newlines are different)

### For development, summarized:
```
cabal run CoulAdj-Hs image.png results.tsv
```
### With the `--dont-relate-diagonals` option:
```
cabal run CoulAdj-Hs --dont-relate-diagonals image.png results.tsv
```

The `cabal run` command will first build the program, unless there was no changes, 
and then execute it.

### To just build:
```
cabal build
```

### To just execute:
```
cabal exec CoulAdj-Hs image.png results.tsv
```

## Benchmarking
The benchmark script in python is currently Windows-only because I was lazy
with filepaths.

Also, despite having installed Python, I still can't run it from powershell,
and I need to click the Play button in Visual Studio Code?!?
(Or maybe it just wasn't correctly installed?!?)

# API

## Input 
*   Source image file path
*   Destination file path
*   Option(s)
    * `--dont-relate-diagonals`
        * If present, only consider as adjacent the four (4) neighbours with
        a common edge. (top, bottom, left, and right neighbours)
        * By default, all 8 neighbours are considered adjacent.


## Output
*   TSV File

### TSV File
*   Tab-separated values (tsv)
    *   [Summary on Wikipedia](https://en.wikipedia.org/wiki/Tab-separated_values) 
    *   [Official specifications](https://www.iana.org/assignments/media-types/text/tab-separated-values)

*   Data will be organized like this:

    |r  |g  |b  |a  |adj_r|adj_g|adj_b|adj_a|
    |---|---|---|---|-----|-----|-----|-----|
    |0  |32 |64 |128|0    |0    |0    |255  |
    |0  |32 |64 |255|0    |0    |0    |255  |
    |0  |32 |64 |255|0    |32   |0    |255  |
    |0  |64 |0  |255|0    |0    |0    |255  |

*   The alpha column will always be included in the output. Images without an alpha channel
will get an alpha value at full opacity.

    |r  |g  |b  |a  |adj_r|adj_g|adj_b|adj_a|
    |---|---|---|---|-----|-----|-----|-----|
    |0  |32 |64 |255|0    |0    |0    |255  |
    |0  |32 |64 |255|0    |32   |0    |255  |
    |0  |64 |0  |255|0    |0    |0    |255  |


*   The rows will be sorted in ascending order.

    |r  |g  |b  |a  |adj_r|adj_g|adj_b|adj_a|
    |---|---|---|---|-----|-----|-----|-----|
    |0  |32 |64 |255|0    |0    |0    |255  |
    |0  |32 |64 |255|0    |32   |0    |255  |
    |0  |32 |64 |255|0    |128  |0    |255  |
    |0  |64 |0  |255|0    |0    |0    |255  |
    |32 |0  |0  |255|0    |0    |0    |255  |
    |255|0  |0  |255|0    |0    |0    |255  |

*   Symmetric relations will be included;
if A is adjacent to B, then B is adjacent to A, 
so this single relation will generate two rows.

    |r  |g  |b  |a  |adj_r|adj_g|adj_b|adj_a|
    |---|---|---|---|-----|-----|-----|-----|
    |0  |0  |0  |255|0    |64   |0    |255  |
    |0  |64 |0  |255|0    |0    |0    |255  |

*   Reflexive relations will *not* be included;
a color cannot be adjacent with itself.

*   Colors that differ only in their alpha value are considered distinct.

    |r  |g  |b  |a  |adj_r|adj_g|adj_b|adj_a|
    |---|---|---|---|-----|-----|-----|-----|
    |0  |0  |0  |128|0    |0    |0    |255  |
    |0  |0  |0  |255|0    |0    |0    |128  |

*   Columns will appear in this order:
    - Red
    - Green
    - Blue
    - Alpha
    - Adjacent Red
    - Adjacent Green
    - Adjacent Blue
    - Adjacent Alpha

*   The first row will contain the column names.
*   The column names will be:

    |Column Name|Color Channel  |
    |-----------|---------------|
    | `r`       |Red            |
    | `g`       |Green          |
    | `b`       |Blue           |
    | `a`       |Alpha          |
    | `adj_r`   |Adjacent Red   |
    | `adj_g`   |Adjacent Green |
    | `adj_b`   |Adjacent Blue  |
    | `adj_a`   |Adjacent Alpha |

*   The line-endings may be either in Windows (CRLF) or Unix (LF) style.

