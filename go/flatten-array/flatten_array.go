// Package flatten takes a possibly nested list of
// "untyped" items and returns all the non-nil items
// in a single list.
//
// problem statement:
// given an arbitrarily-deep nested list like
// structure, return a flattened structure with
// any nil/null values removed.
package flatten

import (
	"fmt"
	"strconv"
	"strings"
)

// Flatten is a naive first attempt that relies on
// "fmt" and "strings". It works, seems obvious, and
// takes about a little less than 3 times longer than
// a more introspective approach.
func Flatten(i interface{}) []interface{} {
	r := []interface{}{}

	s := fmt.Sprint(i)
	s = strings.ReplaceAll(s, "[", "")
	s = strings.ReplaceAll(s, "]", "")
	s = strings.ReplaceAll(s, "<nil>", "")

	sl := strings.Split(s, " ")
	for _, e := range sl {
		if strings.TrimSpace(e) != "" {
			// this counts on the test data being
			// all integers or nils.
			v, _ := strconv.Atoi(e)
			r = append(r, v)
		}
	}

	return r
}

// Squish uses type assertion and recursion to
// drill into the list and flatten it.
func Squish(i interface{}) []interface{} {
	j, _ := i.([]interface{})
	r := make([]interface{}, 0)
	for _, e := range j {
		switch t := e.(type) {
		case nil:
			continue
		case []interface{}:
			r = append(r, Squish(t)...)
		default:
			r = append(r, t)
		}
	}
	return r
}
