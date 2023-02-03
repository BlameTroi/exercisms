// Package allyourbase converts numbers of arbitrary size from one base to another.
package allyourbase

import "errors"

var ErrConvertInputBase = errors.New("input base must be >= 2")
var ErrConvertOutputBase = errors.New("output base must be >= 2")
var ErrConvertBadDigits = errors.New("all digits must satisfy 0 <= d < input base")

// ConvertToBase converts from one base to another.
func ConvertToBase(ibase int, idigits []int, obase int) ([]int, error) {

	// guards and special cases
	err := checkArguments(ibase, obase, idigits)
	if err != nil {
		return []int{}, err
	}
	if len(idigits) == 0 {
		return []int{0}, nil
	}

	// convert digit slice to usable value
	ivalue := 0
	for _, d := range idigits {
		ivalue = ivalue*ibase + d
	}

	// determine the staring power of the output base
	ovalue := ivalue
	opower, p := 1, 1
	for opower <= ovalue {
		p = opower
		opower *= obase
	}
	opower = p

	// convert each digit into the output base, starting from
	// the most significant digt.
	odigits := make([]int, 0)
	for opower > 1 {
		d := ovalue / opower
		ovalue %= opower
		opower /= obase
		odigits = append(odigits, d)
	}

	// last digit, units
	odigits = append(odigits, ovalue)

	return odigits, nil
}

// Check the input arguments to ConvertToBase and report any errors.
func checkArguments(ibase int, obase int, idigits []int) error {
	if ibase < 2 {
		return ErrConvertInputBase
	}
	if obase < 2 {
		return ErrConvertOutputBase
	}
	for _, d := range idigits {
		if d < 0 || d >= ibase {
			return ErrConvertBadDigits
		}
	}
	return nil
}
