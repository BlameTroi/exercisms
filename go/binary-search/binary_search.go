// Package binarysearch provides a simple binary search implementation.
package binarysearch

// SearchInts searches a slice of ints for a specific value,
// returning the value's index if found, or -1 if not found.
func SearchInts(s []int, k int) int {
	if len(s) == 0 {
		return -1
	}
	lo := 0
	hi := len(s) - 1
	mid := (hi - lo) / 2
	for {
		if s[mid] == k {
			return mid
		} else if s[mid] < k {
			lo = mid + 1
		} else {
			hi = mid - 1
		}
		if hi < lo {
			return -1
		}
		mid = lo + (hi-lo)/2
	}
}
