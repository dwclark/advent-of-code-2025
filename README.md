# Advent of Code 2025

Notes from advent of code 2025

## [Day 01](src/day-01.lisp)

Not a great start to 2025. I did part 1 using modular arithmetic. On part 2 I confused myself with doing the counts because of the special cases: landing on zero or not, going negative or positive. I ended up just coding a state function which keeps track of both where the pointer is and how many times it had pointed at zero. It works because the input data doesn't have any crazy numbers. That state function solves both parts.

I did fix the function that gets the data files from the input directory so that it figures out its location by itself. Now I don't have to set the `*input-directory*` variable.

I did spend quite a bit of time later trying to look for a more arithmetic solution. However, the special cases kept piling up. Unless there is some special calls to functions like `mod`, `rem`, `truncate`, etc., it seems like the state approach is the simpler approach given that the numbers in the data set are small.
