// Package listops provides several basic list operators that
// might be seen in a functional language (such as SML).
//
// The original lists are not altered by these methods. New
// lists are returned.
package listops

// Types to improve readability:
type IntList []int
type binFunc func(int, int) int
type predFunc func(int) bool
type unaryFunc func(int) int

// Fold over a list from right to left, applying fn to
// produce a single value.
func (self IntList) Foldr(fn binFunc, initial int) int {
	accum := initial
	for _, item := range self.Reverse() {
		accum = fn(item, accum)
	}
	return accum
}

// Fold over a list from left to right, applying fn to
// produce a single value.
func (self IntList) Foldl(fn binFunc, initial int) int {
	accum := initial
	for _, item := range self {
		accum = fn(accum, item)
	}
	return accum
}

// Append the contents of another IntList to this one.
func (self IntList) Append(other IntList) IntList {
	if len(self) == 0 && len(other) == 0 {
		return []int{}
	}
	// todo: is there a way to allocate the space (capacity) up
	// front and then just assign into a slot using an index?
	// var result [len(self)+len(other)]int will not compile.
	var result IntList
	for _, item := range self {
		result = append(result, item)
	}
	for _, item := range other {
		result = append(result, item)
	}
	return result
}

// Concat is misnamed. It takes a list of IntLists and
// then appends the contents of those lists to the self
// list.
func (self IntList) Concat(others []IntList) IntList {
	if len(self) == 0 && len(others) == 0 {
		return []int{}
	}
	var result IntList
	// todo: there's a better way to copy using slices i bet
	for _, item := range self {
		result = append(result, item)
	}
	for _, other := range others {
		result = result.Append(other)
	}
	return result
}

// Filter returns all the elements of self that satisfy a
// predicate.
func (self IntList) Filter(fn predFunc) IntList {
	if len(self) == 0 {
		return self
	}
	filtered := []int{}
	for _, item := range self {
		if fn(item) {
			filtered = append(filtered, item)
		}
	}
	return filtered
}

// Return the Length of the IntList.
func (self IntList) Length() int {
	return len(self)
}

// Apply the supplied function to every element of self,
// returning a new list.
func (self IntList) Map(fn unaryFunc) IntList {
	if len(self) == 0 {
		return self
	}
	mapped := []int{}
	for _, item := range self {
		mapped = append(mapped, fn(item))
	}
	return mapped
}

// Reverse the order of items in self.
func (self IntList) Reverse() IntList {
	if len(self) == 0 {
		return self
	}
	reversed := []int{}
	for i := len(self) - 1; i >= 0; i-- {
		reversed = append(reversed, self[i])
	}
	return reversed
}
