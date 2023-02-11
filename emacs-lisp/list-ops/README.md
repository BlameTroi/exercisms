# List Ops

Welcome to List Ops on Exercism's Emacs Lisp Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Instructions

Implement basic list operations.

In functional languages list operations like `length`, `map`, and `reduce` are very common.
Implement a series of basic list operations, without using existing functions.

The precise number and names of the operations to be implemented will be track dependent to avoid conflicts with existing names, but the general operations you will implement include:

- `append` (*given two lists, add all items in the second list to the end of the first list*);
- `concatenate` (*given a series of lists, combine all items in all lists into one flattened list*);
- `filter` (*given a predicate and a list, return the list of all items for which `predicate(item)` is True*);
- `length` (*given a list, return the total number of items within it*);
- `map` (*given a function and a list, return the list of the results of applying `function(item)` on all items*);
- `foldl` (*given a function, a list, and initial accumulator, fold (reduce) each item into the accumulator from the left using `function(accumulator, item)`*);
- `foldr` (*given a function, a list, and an initial accumulator, fold (reduce) each item into the accumulator from the right using `function(item, accumulator)`*);
- `reverse` (*given a list, return a list with all the original items, but in reversed order*);

* `list-empty-p` (*given a list, return if the list is empty*)
* `list-sum` (*given a list of numbers, return the sum of all elements*)

## Source

### Created by

- @fapdash