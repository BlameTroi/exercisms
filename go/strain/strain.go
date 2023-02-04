// Package strain provides collection filtering on some arrays.
package strain

// Supported types.
type Ints []int
type Lists [][]int
type Strings []string

// The Keep method returns a new collection of each entry
// that satisfies a predicate function.
func (ci Ints) Keep(f func(int) bool) Ints {
	if ci == nil {
		return Ints(nil)
	}
	co := Ints{}
	for _, i := range ci {
		if f(i) {
			co = append(co, i)
		}
	}
	return co
}

// The discard method returns a new collection of each entry
// that fails a predicate function.
func (ci Ints) Discard(f func(int) bool) Ints {
	if ci == nil {
		return Ints(nil)
	}
	co := Ints{}
	for _, i := range ci {
		if !f(i) {
			co = append(co, i)
		}
	}
	return co
}

// The Keep method returns a new collection of each entry
// that satisfies a predicate function.
func (ci Lists) Keep(f func([]int) bool) Lists {
	if ci == nil {
		return Lists(nil)
	}
	co := Lists{}
	for _, i := range ci {
		if f(i) {
			co = append(co, i)
		}
	}
	return co
}

// The Keep method returns a new collection of each entry
// that satisfies a predicate function.
func (ci Strings) Keep(f func(string) bool) Strings {
	if ci == nil {
		return Strings(nil)
	}
	co := Strings{}
	for _, i := range ci {
		if f(i) {
			co = append(co, i)
		}
	}
	return co
}
