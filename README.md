# Advent of Code 2025

Notes from advent of code 2025

## [Day 01](src/day-01.lisp)

Not a great start to 2025. I did part 1 using modular arithmetic. On part 2 I confused myself with doing the counts because of the special cases: landing on zero or not, going negative or positive. I ended up just coding a state function which keeps track of both where the pointer is and how many times it had pointed at zero. It works because the input data doesn't have any crazy numbers. That state function solves both parts.

I did fix the function that gets the data files from the input directory so that it figures out its location by itself. Now I don't have to set the `*input-directory*` variable.

I did spend quite a bit of time later trying to look for a more arithmetic solution. However, the special cases kept piling up. Unless there is some special calls to functions like `mod`, `rem`, `truncate`, etc., it seems like the state approach is the simpler approach given that the numbers in the data set are small.

## [Day 02](src/day-02.lisp)

The main key to solving both parts is to generate the numbers with the correct patterns that are be within the bounds, rather than searching for numbers that match the pattern. Once the numbers are generated collect them and add up the values.

## [Day 03](src/day-03.lisp)

The basic algorithm to find the largest number of n digits is as follows:

1. Starting at index = 0, find the first number, with its index, that is largest **and** leaves at least n-1 digits to the right
2. Accumulate number * 10 ^ (n-1) into an accumulator
3. Set the index to the found index + 1
4. Set n = n - 1, go back to 1. Repeat while n > 0

Basically, you are trying to assemble the biggest number possible as you proceed right. You always want to find the biggest number that leaves enough digits to form the rest of the number. It doesn't matter which digits remain, if the current number is biggest, the resulting number will always be the biggest possible.

## [Day 05](src/day-05.lisp)

Part 1 should be easy to follow.

In part two the algorithm to find the number of fresh ids is:

1. Sort the ranges by the lower value in the ranges.
2. For each range, go through all previous ranges and adjust the lower range value (of the current range) to remove any ids that were previously accounted for until:
   * Either you arrive at the current range
   * The range becomes invalid, i.e. the lower range value is > the upper range value
3. Once a range becomes invalid, don't use it to adjust the ranges of any other range, i.e. ignore it when adjusting ranges.
