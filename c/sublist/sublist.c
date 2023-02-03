// exercism sublist -- are either of two lists sublists
// of the other?
//
// list a is a sublist of list b if by dropping 0 or more
// elements from the front and/or back, you get a list
// that's completely equal to list a.
//
// t.brumley, june 2022.

#include "sublist.h"
#include <stdbool.h>

// forward definitions
static
bool areEqual(
		int *left,
		int *right,
		size_t nLeft,
		size_t nRight);

static
bool containedIn(
		int *left,
		int *right,
		size_t nLeft,
		size_t nRight);

// is the left list equal to the right?
static
bool areEqual(
		int *left,
		int *right,
		size_t nLeft,
		size_t nRight) {
	if (nLeft != nRight)
		return false;
	int *x, *y;
	x = left;
	y = right;
	for(size_t i = 0; i < nLeft; i++)
		if (*x++ != *y++)
			return false;
	return true;
}

// is the left list contained in the right?
static
bool containedIn(
		int *left,
		int *right,
		size_t nLeft,
		size_t nRight) {

	if (areEqual(left, right, nLeft, nRight))
		return true;

	if (nLeft == 0 && nRight != 0)
		return true;

	if (nLeft > nRight)
		return false;

	// match left against first nLeft of right, checking for equal
	while (nRight >= nLeft) {
		if (areEqual(left, right, nLeft, nLeft))
			return true;
		right++;
		nRight--;
	}

	return false;
}

// api for the comparison, lists are integer arrays with length
// explicitly passed.
comparison_result_t check_lists(
		int *compareList,
		int *baseList,
		size_t nCompareList,
		size_t nBaseList) {

	// if the lists are the same length, then this is a straight equality check
	if (nCompareList == nBaseList) {
		if (areEqual(compareList, baseList, nCompareList, nBaseList))
			return EQUAL;
		else
			return UNEQUAL;
	}

	// if one list is empty and the other is not, the empty is a sublist of the other
	if (nCompareList == 0)
		return SUBLIST;
	if (nBaseList == 0)
		return SUPERLIST;

	// both lists have elements, see if one is contained in the other
	if (containedIn(compareList, baseList, nCompareList, nBaseList))
		return SUBLIST;
	if (containedIn(baseList, compareList, nBaseList, nCompareList))
		return SUPERLIST;

	// no hope
	return UNEQUAL;
}



