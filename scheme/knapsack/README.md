# Knapsack

Welcome to Knapsack on Exercism's Scheme Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

In this exercise, let's try to solve a classic problem.

Bob is a thief. After months of careful planning, he finally manages to
crack the security systems of a high-class apartment.

In front of him are many items, each with a value (v) and weight (w). Bob,
of course, wants to maximize the total value he can get; he would gladly
take all of the items if he could. However, to his horror, he realizes that
the knapsack he carries with him can only hold so much weight (W).

Given a knapsack with a specific carrying capacity (W), help Bob determine
the maximum value he can get from the items in the house. Note that Bob can
take only one of each item.

All values given will be strictly positive. Items will be represented as a
list of pairs, `wi` and `vi`, where the first element `wi` is the weight of
the *i*th item and `vi` is the value for that item.

For example:

Items: [
  { "weight": 5, "value": 10 },
  { "weight": 4, "value": 40 },
  { "weight": 6, "value": 30 },
  { "weight": 4, "value": 50 }
]

Knapsack Limit: 10

For the above, the first item has weight 5 and value 10, the second item has
weight 4 and value 40, and so on.

In this example, Bob should take the second and fourth item to maximize his
value, which, in this case, is 90. He cannot get more than 90 as his
knapsack has a weight limit of 10.


## Track Specific Notes

In the scheme version the aruguments are the `capacity` of the knapsack and a list of the `weights` and a list of the `values`\.
It won't be necessary to validate the input \-\- the
test inputs have valid values and same length lists\.

## Running and testing your solutions


### From the command line

Simply type `make chez` if you're using ChezScheme or `make guile` if you're using GNU Guile\.
Sometimes the name for the scheme binary on your system will differ from the defaults\.
When this is the case, you'll need to tell make by running `make chez chez=your-chez-binary` or `make guile guile=your-guile-binary`\.

### From a REPL

* Enter `(load "test.scm")` at the repl prompt\.
* Develop your solution in `knapsack.scm` reloading as you go\.
* Run `(test)` to check your solution\.

### Failed Test Cases

If some of the test cases fail, you should see the failing input and the expected output\.
The failing input is presented as a list because the tests call your solution by `(apply knapsack input-list)`\.
To learn more about `apply` see [The Scheme Programming Language -- Chapter 5](https://www.scheme.com/tspl4/control.html#./control:h1)

## Source

### Created by

- @jitwit

### Contributed to by

- @guygastineau

### Based on

Wikipedia - https://en.wikipedia.org/wiki/Knapsack_problem