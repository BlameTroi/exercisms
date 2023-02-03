// Package twofer implements a simple text response.

package twofer

import "strings"

// ShareWith returns a string with the parameter, a name,
// embedded. If the name is blank or empty then "you" is
// used instead.
func ShareWith(name string) string {
	if len(strings.TrimSpace(name)) == 0 {
		name = "you"
	}
	return "One for " + name + ", one for me."
}
