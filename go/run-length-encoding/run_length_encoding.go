// Package encode provides run length encoding of
// strings.
//
// Only blanks and upper/lower case ascii letters
// are supported. The spec does not provide for
// error handling so any errors are ignored.
package encode

import (
	"strconv"
	"strings"
)

// TODO: Output strings should be assembled using a
// Writer.

// RunLengthEncode does a simple encoding of a string
// of alphabetic characters and spaces.
func RunLengthEncode(in string) string {
	if len(in) < 2 {
		return in
	}

	var out string
	var run int
	var last rune

	for _, curr := range in {
		// The first time is always awkward.
		if last == '\000' {
			run, last = 1, curr
			continue
		}

		// Accumulate run.
		if curr == last {
			run++
			continue
		}

		// Write the run if there was one.
		if run > 1 {
			out += strconv.Itoa(run)
		}

		// And finally write character
		out += string(last)

		// Reset/remember.
		run, last = 1, curr
	}

	// Write the last run if there was one
	if run > 1 {
		out += strconv.Itoa(run)
	}

	// Final character.
	out += string(last)
	return out
}

// RunLengthDecode does a simple decoding of a string
// that has been encoded by RunLengthEncode. As per
// the spec, any digits are treated as "repeat the
// following character" markers. No error handling
// was specified so malformed input will produce
// bad output.
func RunLengthDecode(in string) string {
	if len(in) < 2 {
		return in
	}

	var out string
	var run int

	for _, curr := range in {

		// Assemble any digits into the run
		if curr >= '0' && curr <= '9' {
			run = run*10 + int(curr) - int('0')
			continue
		}

		// Non digit, if there wasn't a run, just write
		// it out.
		if run == 0 {
			out += string(curr)
			continue
		}

		// Non digit with run, write the appropriate
		// number of characters and reset run.
		//
		// While a run of '1' will process correctly,
		// we should never see one coming from the
		// encoder.
		out += strings.Repeat(string(curr), run)
		run = 0
	}

	// If run is not zero here, the incoming string was
	// malformed; it had trailing digits. This error is
	// ignored and the digits are lost.
	return out
}
