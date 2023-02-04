// Package luhn validates a checkdigited number using
// Luhn's formula.
package luhn

// Valid takes a string s and determines if the number
// it contains is a valid checkdigted number. Only
// blanks and digits are allowed. Blanks are quietly
// ignored.
//
// Working from the last digit to the first, sum the
// digits (for odd slots), and double the digits (for
// even slots) modulo 9.
//
// ' #### #### #### '
//   EOEO EOEO EOEO
//
// At least 2 digits must be found in the string.
func Valid(s string) bool {
	if len(s) < 2 {
		return false
	}
	odd := true
	sum := 0
	digits := 0
	for i := len(s) - 1; i >= 0; i-- {
		if s[i] == ' ' {
			continue
		}
		if s[i] < '0' || s[i] > '9' {
			return false
		}
		digits++
		if odd {
			sum += int(s[i] - '0')
		} else {
			if s[i] < '5' {
				sum += int(s[i]-'0') * 2
			} else {
				sum += int(s[i]-'0')*2 - 9
			}
		}
		odd = !odd
	}
	return digits > 1 && sum%10 == 0
}
