# CoulAdj-TestSamples

This repository contains an elaborate set of images meant to be used when testing the correctness
and performance of a CoulAdj implementation.

These images were created because previously, the big images used to test the performances
were proprietary and couldn't be freely distributed.

All samples should produce the same output, and this output should be identical, 
byte for byte(\*), to `golden.tsv`. (\*: The line endings may differ depending on the 
platform where the output was produced.)

## Archive files

**Warning: Archives were compressed for a reason.**

This table lists the approximate size that an archive will take once
it is uncompressed:

| Archive File | Uncompressed Size |
|--------------|-------------------|
| `bmp-samples.zip` | 100 MB |
| `prolog-pixels-sizes-32-to-256.7z` | 200 MB |
| `prolog-pixels-size-512.7z` | 600 MB |

## Prolog Support
10 months after its inital creation, this repository was updated to
include sample data adapted for the Prolog implementation.

The Prolog language has many particularities, one of them being that it's not
meant to take image files in input, but instead text files that describe 
"facts". Hence, we decided that for CoulAdj-Prolog, another language would be
used to take the image file in input, create a text file with the pixel values,
and pass that text file in input to the Prolog program.

The problem is that the goal of these test samples is to have them 
human-verifiable. This can be trivially done by sighted humans who can look
at the enlarged preview available in this Readme. However, the canonical
input to the Prolog program is a text file. And the smallest size in the regular
samples has 72 pixels, meaning 72 rows of text.

72 rows of text is too much for manual verification, especially since a lot of
rows and columns are duplicates.

Hence, the Prolog samples is an image of 5 by 6, for a total of 30 pixels, or
30 rows of text. There is still a duplicate row in this sample, so we could
have reduced the sample to 5 by 5, for 25 pixels, but then we would have an
equal amount of rows and columns, and we decided that 5 rows of text was
an acceptable cost to keep a different number of rows and columns.

The Prolog sample is meant to have the same adjacencies, and surface the same
bugs, as the regular samples.

This Readme has been fully reviewed to make sure that all the
information is correct and up-to-date. Note that we only mention the Prolog
sample when it differs, so any reference to "all" samples 
includes the Prolog sample.


# Preview

The sample of size 1 looks like the image below. This preview is 32 times
bigger than the actual sample.

![sample-preview](./doc/sample-preview.png)

Actual size: ![sample-1](sample-size-1.png)

NB: In all samples, the top-right red square was made to always be 1 by 1 pixel.
This is why you can hardly see the red in the top-right when looking at the bigger
samples.

## Prolog
The Prolog sample looks like the image below. This preview is 32 times
bigger than the actual sample.

![sample-preview-prolog](./doc/sample-preview-prolog.png)

Actual size: ![sample-p](./prolog/sample-size-p.png)

# Sizes
In the preview, you can see that the colours are assigned by squares. We will
call these squares "cells".

The **size** of a (non-Prolog) sample is the **length of a cell in pixels**. Since the sample
is a grid of 9 by 8 cells, the sample of size 1 is 9 by 8 pixels, the
sample of size 2 is 18 by 16 pixels, etc etc... The Prolog sample is an
exception, hence it is of size `p`. ("p" for "Prolog")

The size closest to the intended real-world use-case is 512, with ~18M pixels.
As such, execution time at size 512 is the "canonical" metric, 
and it should be used when comparing different implementations.

The included sizes are:

| Size | Width | Height |  Nb Pixels | Notes |
|-----:|------:|-------:|-----------:|:----------:|
|    p |     5 |      6 |         30 | Prolog sample |
|    1 |     9 |      8 |         72 |
|    2 |    18 |     16 |        288 |
|    4 |    36 |     32 |      1,152 |
|    8 |    72 |     64 |      4,608 |
|   16 |   144 |    128 |     18,432 |
|   32 |   288 |    256 |     73,728 |
|   64 |   576 |    512 |    294,912 |
|  128 |  1152 |   1024 |  1,179,648 |
|  256 |  2304 |   2048 |  4,718,592 |
|  **512** |  **4608** |   **4096** | **18,874,368** | **Canonical** |


The high quantity of sizes is meant to verify if the implementation went
[quadratic](https://en.wikipedia.org/wiki/Time_complexity#Sub-quadratic_time) 
in languages with a slow execution time.
~~(Our best Power Query implementation to date takes a whooping 8 minutes. 
And yes, this is with a sub-quadratic implementation.)~~
(Our Power Query implementation is now under 30 seconds!! 😁)

> Each (non-Prolog) size has 4 times the amount of pixels than the previous size.



# Correct results
The correct results, for all samples, 
_with additional columns to name the colours_, are:

|   r |   g |   b |   a | adj_r | adj_g | adj_b | adj_a | _Colour_  | _adj_Colour_ |
|----:|----:|----:|----:|------:|------:|------:|------:|-----------|--------------|
|  10 |  20 | 230 | 252 |    10 |   220 |    30 |   250 | _Blue_    | _Green_      |
|  10 |  20 | 230 | 252 |    10 |   220 |   230 |   248 | _Blue_    | _Cyan_       |
|  10 |  20 | 230 | 252 |   210 |    20 |    30 |   246 | _Blue_    | _Red_        |
|  10 |  20 | 230 | 252 |   210 |    20 |   230 |   244 | _Blue_    | _Magenta_    |
|  10 |  20 | 230 | 252 |   210 |   220 |    30 |   242 | _Blue_    | _Yellow_     |
|  10 | 220 |  30 | 250 |    10 |    20 |   230 |   252 | _Green_   | _Blue_       |
|  10 | 220 |  30 | 250 |    10 |   220 |   230 |   248 | _Green_   | _Cyan_       |
|  10 | 220 |  30 | 250 |   210 |    20 |    30 |   246 | _Green_   | _Red_        |
|  10 | 220 |  30 | 250 |   210 |   220 |    30 |   242 | _Green_   | _Yellow_     |
|  10 | 220 | 230 | 248 |    10 |    20 |   230 |   252 | _Cyan_    | _Blue_       |
|  10 | 220 | 230 | 248 |    10 |   220 |    30 |   250 | _Cyan_    | _Green_      |
|  10 | 220 | 230 | 248 |   210 |    20 |   230 |   244 | _Cyan_    | _Magenta_    |
|  10 | 220 | 230 | 248 |   210 |   220 |    30 |   242 | _Cyan_    | _Yellow_     |
| 210 |  20 |  30 | 246 |    10 |    20 |   230 |   252 | _Red_     | _Blue_       |
| 210 |  20 |  30 | 246 |    10 |   220 |    30 |   250 | _Red_     | _Green_      |
| 210 |  20 |  30 | 246 |   210 |    20 |   230 |   244 | _Red_     | _Magenta_    |
| 210 |  20 | 230 | 244 |    10 |    20 |   230 |   252 | _Magenta_ | _Blue_       |
| 210 |  20 | 230 | 244 |    10 |   220 |   230 |   248 | _Magenta_ | _Cyan_       |
| 210 |  20 | 230 | 244 |   210 |    20 |    30 |   246 | _Magenta_ | _Red_        |
| 210 |  20 | 230 | 244 |   210 |   220 |    30 |   242 | _Magenta_ | _Yellow_     |
| 210 | 220 |  30 | 242 |    10 |    20 |   230 |   252 | _Yellow_  | _Blue_       |
| 210 | 220 |  30 | 242 |    10 |   220 |    30 |   250 | _Yellow_  | _Green_      |
| 210 | 220 |  30 | 242 |    10 |   220 |   230 |   248 | _Yellow_  | _Cyan_       |
| 210 | 220 |  30 | 242 |   210 |    20 |   230 |   244 | _Yellow_  | _Magenta_    |


# Rules

All test samples obey these rules:
1. Width and height are different.
1. Values in the red channel must be either 10 or 210
1. Values in the green channel must be either 20 or 220
1. Values in the blue channel must be either 30 or 230
1. Values in the alpha channel (if present) must be between 240 and 254 (inclusive).
Additionally, the alpha value must represent a unique RGB combination.
1. More specifically, only these colours are allowed:
    |   r |   g |   b |   a | Name    | ID | r (hex) | g (hex) | b (hex) | a (hex) |
    |----:|----:|----:|----:|---------|----|--------:|--------:|--------:|--------:|
    |  10 |  20 | 230 | 252 | Blue    | 52 |  0a     |  14     |  e6     |  fc     |
    |  10 | 220 |  30 | 250 | Green   | 50 |  0a     |  dc     |  1e     |  fa     |
    |  10 | 220 | 230 | 248 | Cyan    | 48 |  0a     |  dc     |  e6     |  f8     |
    | 210 |  20 |  30 | 246 | Red     | 46 |  d2     |  14     |  1e     |  f6     |
    | 210 |  20 | 230 | 244 | Magenta | 44 |  d2     |  14     |  e6     |  f4     |
    | 210 | 220 |  30 | 242 | Yellow  | 42 |  d2     |  dc     |  1e     |  f2     |
1. When looking at the image from top-left to top-right, and then from top to bottom,
the colours must appear such that those with the lower alpha values appear first.

# Rules of thumb

When looking at the output...
* In each row, the digit of the tens should always be increasing from left column to right column.
* In the first alpha column (`a`), each row should have a value greater or equal than the row below.
* In the second alpha column (`adj_a`), the above should be true when grouping rows by `a` value.


# Rationales

The rules regarding what colour is allowed were designed to accomplish these goals:

## Correct ordering of RGBA columns

The old test samples we created had values of either 0 or 255 in all channels. This complicated verifying
that the program had correctly identified the RGBA channels. 

Now, you can tell to which channel a value belongs by looking at the tens digit of
the value. If it's 1 (10 or 210), then red. 2 is green, 3 is blue, and 4 & 5 are alpha.

This is the same order the columns should be appearing in the output, and also the
same order that must be used when comparing the colours to sort the rows.

## Quick identification of what colour a pixel should be.

Since the alpha value is unique for each colour, we only need to look at the alpha
to know which colour is supposed to be represented by this pixel.

Before, we had to order and compare the 0s and 255s from all 3 RGB channels.

> For example, compare (0,255,255,255) vs (255,255,0,255). Can you tell which is yellow and which
> is cyan? 
>
> Now, we have (10,220,230,248) vs (210,220,30,242), and you only need to
> know that cyan is 248 and yellow is 242.

## Correct sorting of colours

We want the sort to be done by channels. ie, (10, 20, 230) must appear (in the ouput)
before (10, 220, 30). This is why we only encoded the ID of the colour in the alpha
channel. Had we encoded all the channels, these two colours would then be something
like (16, 26, 236) and (15, 225, 35), making that specifications unverifiable.

Furthermore, the IDs to encode in the alpha channel were chosen so that if the
output had been sorted, then we only need to check the `a` column is also sorted.

However, we didn't want this encoding to have an impact on the sorting itself,
so the IDs were chosen so that the alpha column should always be sorted from 
greater to lower value. This way, if the alpha was ever used to sort two colours,
it would be visible in the ouput.

## A sort was actually performed

Some languages use structures that remember the order of insertion. To verify
that the output was actually sorted and isn't depending on the insertion order,
the images are made so that the insertion order would give an obviously incorrect
result, because it would interfere with the expected ordering of the alpha column.

> For example, Yellow with `(210,220,30)` will be sorted last. Because it will
> be last, it was given the `242` alpha value, the lowest, and was made to be
> first color encountered at pixel `(row 0, col 0)`.
>
> The second-to-last colour in the sort is Magenta with `(210,20,230)`. 
> Magenta was thus given `244` alpha value, and assigned to be the second
> colour to encounter when the main loop gets to `(row 0, col 5)` in the size 1 sample.
>
> A sort by order of insertion would then produce this (non-conformant) output:
> * `(210,220, 30,242)`
> * `(210, 20,230,244)`

# Troubleshooting FAQ

## There are too many rows in the output
You are probably looping around, with the top row thinking it's adjacent to the
bottom row, and likewise for left vs right columns.

When checking the validity of neighbour coordinates, do you check that 
`neighRow` and `neighCol` are both greater or equal to `0`? If you don't, you will be
accessing negative indexes, and some language treat index `-1` as "first item from the end".

This [happened](https://github.com/AmeliaSZK/CoulAdj-JS/commit/33853ef21b3e6fa1d6730f2f72020fe25e07610d) in the JavaScript implementation.


## The first alpha column (`a`) is sorted in ascending order
Are you sure the colours are compared correctly? Some libraries use ARGB under
the hood instead of RGBA, and you'll need to specify a custom compare function.

This happened during the Qt implementation, because [QRgb](https://doc.qt.io/qt-5/qcolor.html#QRgb-typedef)
is an unsigned integer formatted as `#AARRGGBB`


## My output is the same as `golden.tsv`, but git diff says the files are different?!
Two (known) possibilities:

1. You joined the lines with a newline character instead of appending a newline
character at the end of each line. If your output doesn't end with a blank line,
you have this problem. The terminal newline is essential because it is
required by the [TSV specification](https://www.iana.org/assignments/media-types/text/tab-separated-values):
    >record   ::= field [TAB field]+ EOL

1. Your output was produced on Windows with a CRLF line ending, and you are comparing
with a `golden.tsv` that has an LF (Linux/Mac) line ending. This is not a problem with your implementation;
feel free to compare with a `golden.tsv` converted to CRLF line endings, or use
any other workaround.


## I am crashing due to out-of-bounds indexes
Verify that you didn't mix up width and height, row and column, X and Y.

Libraries usually have (0,0) in the top left corner, but wether X grows to
the right or to the bottom can diverge. Furthermore, some longstanding conventions
are in opposition. For example, in a cartesian plan, the cell at (row 1, col 2)
will be on (x=2, y=1).

|     | 0 | 1 | 2 |
|-----|---|---|---|
|**0**|   |   |   |
|**1**|   |   | 🎉 |
|**2**|   |   |   |

> This problem is a danger in every implementation, and it is the reason why we create variables
> such as `maxRow` and `nbRows` instead of using `height-1` and `height`.

## I'm getting weird variations in the colours, like `(10,219,31,250)` for Green
The library function that imports the file is probably (*probably*) doing some color management 
shenanigans behind the scene.

At the time of writing these lines, it is an [open issue](https://github.com/AmeliaSZK/CoulAdj-JS/issues/2) 
in the JavaScript implementation.
