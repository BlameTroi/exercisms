// Package romannumerals converts an integer into a string of
// Roman numerals.
package romannumerals

import (
	"errors"
	"strconv"
)

// Function reverse reverses a string.
func reverse(s string) string {
	r := ""
	for _, c := range s {
		r = string(c) + r
	}
	return r
}

// Function romanDigitsByPowersOfTen is a work around for go not having
// array constants.
func romanDigitsByPowersOfTen() [][]string {
	return [][]string{
		{"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"},
		{"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"},
		{"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"},
		{"", "M", "MM", "MMM", "", "", "", "", "", "", ""},
	}
}

// Function ToRomanNumeral returns a string of representation of an int
// in the range 1 <= int <= 3999 using Roman numerals.
func ToRomanNumeral(a int) (string, error) {
	if a < 1 || a > 3999 {
		return "", errors.New("can not convert numbers to roman numerals outside the range of 1 to 3999 inclusive")
	}
	// Get a local copy of the dec->rom digit table.
	decToRom := romanDigitsByPowersOfTen()
	// Process as string for convienience.
	dec, rom := reverse(strconv.Itoa(a)), ""
	for p, d := range dec {
		rom = decToRom[p][d-'0'] + rom
	}
	return rom, nil
}
