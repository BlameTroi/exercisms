# Sieve

Welcome to Sieve on Exercism's Scheme Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

You bought a big box of random computer parts at a garage sale.
You've started putting the parts together to build custom computers.

You want to test the performance of different combinations of parts, and decide to create your own benchmarking program to see how your computers compare.
You choose the famous "Sieve of Eratosthenes" algorithm, an ancient algorithm, but one that should push your computers to the limits.

## Instructions

Your task is to create a program that implements the Sieve of Eratosthenes algorithm to find prime numbers.

A prime number is a number that is only divisible by 1 and itself.
For example, 2, 3, 5, 7, 11, and 13 are prime numbers.

The Sieve of Eratosthenes is an ancient algorithm that works by taking a list of numbers and crossing out all the numbers that aren't prime.

A number that is **not** prime is called a "composite number".

To use the Sieve of Eratosthenes, you first create a list of all the numbers between 2 and your given number.
Then you repeat the following steps:

1. Find the next unmarked number in your list. This is a prime number.
2. Mark all the multiples of that prime number as composite (not prime).

You keep repeating these steps until you've gone through every number in your list.
At the end, all the unmarked numbers are prime.

```exercism/note
[Wikipedia's Sieve of Eratosthenes article][eratosthenes] has a useful graphic that explains the algorithm.

The tests don't check that you've implemented the algorithm, only that you've come up with the correct list of primes.
A good first test is to check that you do not use division or remainder operations.

[eratosthenes]: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
```


## Running and testing your solutions


### From the command line

Simply type `make chez` if you're using ChezScheme or `make guile` if you're using GNU Guile\.
Sometimes the name for the scheme binary on your system will differ from the defaults\.
When this is the case, you'll need to tell make by running `make chez chez=your-chez-binary` or `make guile guile=your-guile-binary`\.

### From a REPL

* Enter `(load "test.scm")` at the repl prompt\.
* Develop your solution in `sieve.scm` reloading as you go\.
* Run `(test)` to check your solution\.

### Failed Test Cases

If some of the test cases fail, you should see the failing input and the expected output\.
The failing input is presented as a list because the tests call your solution by `(apply sieve input-list)`\.
To learn more about `apply` see [The Scheme Programming Language -- Chapter 5](https://www.scheme.com/tspl4/control.html#./control:h1)

## Source

### Created by

- @jitwit

### Based on

Sieve of Eratosthenes at Wikipedia - http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes