// Package isbn provides isbn checkdigit support.
package isbn

// IsValidISBN validates the digits of an ISBN10.
func IsValidISBN(s string) bool {
	if len(s) < 10 {
		return false
	}

	digits := make([]rune, 0, 11)

	for _, r := range s {
		if r >= '0' && r <= '9' {
			digits = append(digits, r-'0')
			continue
		}
		if r == 'X' && len(digits) == 9 {
			digits = append(digits, rune(10))
			continue
		}
	}

	if len(digits) != 10 {
		return false
	}

	chksum := 0
	weight := 10
	for _, r := range digits {
		chksum += int(r) * weight
		weight--
	}

	return chksum%11 == 0
}
