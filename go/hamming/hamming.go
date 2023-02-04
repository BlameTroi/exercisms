// Package hamming calculates the Hamming Distance between
// two strings.
package hamming

import "errors"

// Distance calculates the Hamming Distance between two
// strings. If the strings are not of equal length, an
// error is returned.
func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return 0, errors.New("hamming: strings must be of equal length")
	}
	if len(a) == 0 || a == b {
		return 0, nil
	}
	ham := 0
	for i, xa := range a {
		xb := b[i : i+1]
		if string(xa) != xb {
			ham++
		}
	}
	return ham, nil
}
