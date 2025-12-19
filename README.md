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

## [Day 06](src/day-06.lisp)

Part 1 was easy. Read in the whole file as lists of symbols. Then go column by column applying the operation in the last column.

Part 2 means I had to throw away just reading in the file as symbols, I had to treat them as strings. This lead to lots of off-by-one and alignment issues. The two keys to keeping thing aligned are:

1. Problems begin in the column where the operator is. This means you have to extract the operators and the columns from the last line before processing any other lines.
2. Extracting the strings for a problem is now a matter of getting a subsequence of characters starting where the operator starts. The subsequence ends either 1 character before the next operator OR at the end of the line.

Finally, for a given problem read the columns backwards and extract one character per line to form a new string. Each string can then be converted into a number (ignoring any spaces). Gather those numbers and apply the operator to solve a single problem. Now add up the solutions to all of the problems.

## [Day 08](src/day-08.lisp)

Part 1 was relatively easy.

However, the day as a whole was mainly about following instructions exactly and reading comprehension. It took me forever just to make sense of part 2. At first I followed mechanically what part 2 asked for. However, that was *very* slow. I had to do a couple of small optimizations to make it run quick enough to solve the problem in a reasonable amount of time.

## [Day 09](src/day-09.lisp)

Part 1 was easy. Too easy.

I say too easy because it ended up making it hard for *me* to solve part 2. The problem was I kept wanting to re-use the code in part 1 to solve part 2. What this meant was that I kept using part 1 to get the edges of the polygon...but this is not correct. The only way to get the edges of the polygon is to do exactly as the problem says, go line by line constructing the edges. I kept just using the 1 unit width lines of part 1 *as* the edges. This is of course wrong because it will make edges that bisect the polygon along with the actual edges of the polygon. This means that any inscribed rectangle will end up being smaller than it should because more lines will mean more limitations on the inscribed rectangle.

By the time I figured this out I had spent way too much time on it and had gotten myself way too confused. I resorted to understanding someone else's solution and translating it into common lisp. The fine person whose [work I copied did an excellent job explaining the python code](https://github.com/alexprengere/advent_of_code/blob/master/2025/09/python/main.py). See comments in my code for the key insight into solving the problem.

Things I learned from this problem:

* Code reuse is over rated. Solve the problem at hand and don't be mesmerized by your previous code. It may have nothing to do with the problem at hand.
* It helps to have a picture for these geometric problems. I learned enough gnuplot to draw the polygon represented by the points in the data file. This helped with visualization and understanding what the goal was.
* Common Lisp really doesn't have any good geometry libraries. I did use [this library](https://github.com/Ramarren/cl-geometry) to try and solve the problem. While I did learn some new concepts and ideas from the library (unfortunately, none of them helped me solve the problem) I did come across a nasty bug that made me unable to continue using it to try and solve the problem.
* I need to learn more about computational geometry. To be clear, I doubt there is an algorithm that solves this problem directly. However, learning more about geometrical computations would probably have made me more comfortable programming with geometrical ideas and given me more confidence in exploring the problem space. AOC should be about learning new things, not solving specific problems.

## [Day 10](src/day-10.lisp)

Part 1 was as always pretty straightforward. Use Dijkstra's algorithm to find the quickest combinartion of presses that arrives at the combo.

Part 2 was a doozy. Some notes on the solution:

* I immediately knew that Dijkstra's wasn't going to work.
* I was able to pretty easily solve the sample problems using a recursive solution with memoization.
* I did switch at this point from using fare-memoization to using function-cache for memoizing functions. I also went back and removed fare-memoization from day 07. The main issue that fare-memoization doesn't behave well on recompiles. Secondarily it also doesn't have as much in the way of controlling the caches used.
* I did realize that this was a diophantine style equation, but I don't know how to solve those.
* I eventually found [this reddit page](https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/) which explained how that person solved part 2. I did eventually do a straightforward copy of the python into common lisp.
* The copy is pretty 1 to 1. Nice to see that common lisp is significantly faster than python, 6.5s vs 10.5s.
* I don't like the explanation on the reddit page. I have tried to comment my code to explain the "trick."
* I do think this one is a trick. You have to assume the solution will be of a certain shape. I don't think in general you can assume this for this type of problem if the data were to be chosen differently.
* Lots of people solved this using SAT solvers. I played around with the Z3 python SAT solver. I was able to get it to solve one of the sample problems manually. However, I had to tell it that one of the variables should definitely be zero to get the minimal correct solution. When I only put the restruction that every push had to be >= 0, it did not find a minimal solution. I didn't see any minimization function, but I only played around with it for an hour or so. I should investigate SAT solvers more, they seem like interesting tools.
* I looked for a common lisp SAT solver. They exist, but none appear to support arithmetic. This may be false as a second look at them makes me think that cl-sat just wraps a c based solver which may handle arithmetic.
* The correct solution is I think to use [the Simplex algorithm](https://en.wikipedia.org/wiki/Simplex_algorithm). There does appear to be some form of simplex implemented for common lisp in [some guy's code competition library](https://github.com/privet-kitty/cl-competitive/blob/master/module/simplex-common.lisp). There's also [this simplex library for common lisp](https://github.com/postamar/cl-rational-simplex). This [solution uses simplex](https://github.com/RussellDash332/advent-of-code/blob/main/aoc-2025%2FDay-10%2FPython%2Fmain.py). I read the Wikipedia article on Simplex and it does appear to be best algorithm for solving these kinds of linear systems with constraints.

**Adendum 2025-12-18**

It turns out that there is (of course) a library that does simplex, you just have to know what you are looking for. In fact, it appears to be a very good library that is easy to use: [Common Lisp Linear Programming](https://neil-lindquist.github.io/linear-programming/). It helps to know that you should search from Linear Programming as the more generic term, not Simplex.

In fact, I was able to solve one of the example problems without much research. For example, this:

```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

Can be solved with this:

```lisp
(defvar aoc-problem (parse-linear-problem '(min (= w (+ a b c d e f)))
						   '((<= 0 a)
						     (<= 0 b)
						     (<= 0 c)
						     (<= 0 d)
						     (<= 0 e)
						     (<= 0 f)
						     (= 3 (+ e f))
						     (= 5 (+ b f))
						     (= 4 (+ c d e))
						     (= 7 (+ a b d)))))

(defvar aoc-solution (solve-problem aoc-problem))

(with-solution-variables (w a b c d e f)
   (format t "total: ~A" w)
   (format t "a: ~A, b: ~A, c: ~A, d: ~A, e: ~A, f: ~A" a b c d e f))

;; output ->
;; total: 10
;; a: 1, b: 5, c: 0, d: 1, e: 3, f: 0
```

## [Day 11](src/day-11.lisp)

Part 1. As always pretty easy.

Part 2. This is a pretty common AoC problem, counting paths with large amounts of branching. The keys are always:

* Don't try to form the paths, always just do the counting.
* The solution is always recursive and memoized. The key is to set up the recursive solution so that memoization actually happens.

## [Day 12](src/day-12.lisp)

At first I panicked, another geometrical challenge :(

I did peek at reddit to see if there was a part 2, the format has changed and I simply wanted to know if I was going to be able to finish today. Turns out, no part 2. I did see a cartoon which said the practice inputs are much harder than the actual inputs. That was the clue I needed.

I started look for geometrical algorithms that solved these tetris type problems. I immediately saw that these types of things are NP complete and fall under the general category of bin problems. A stack overflow post was also kind enough to point me to [Skiena's book](https://www.amazon.com/Algorithm-Design-Manual-Computer-Science/dp/3030542556/) on bin problems. Sure enough, Skiena confirmed that these problems are NP complete and that most solutions involving irregular shapes involve packing the items into regular boxes and solving the problem with the boxes.

At this point I guessed that every line would fall into one of two categories: either definitely possible or definitely impossible. I did a quick program to check that one of those cases holders for every region. It turns out that one does hold for each region. At that point, it was mainly a matter of just counting the regions for which a solution is definitely possible.

Definitely a day in which paying attention to what's possible and what the data looks like paid off big.
