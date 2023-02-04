// is a given year a leap year
package leap

// is a given year a leap year as in the gregorian calendar
// rather than a more complex if structure, use guard clauses
// and return as soon as we are sure of the result.
func IsLeapYear(y int) bool {

	if y%400 == 0 { // evenly divisible by 400 is
		return true
	}

	if y%100 == 0 { // evenly divisible by 100 is not
		return false
	}

	if y%4 == 0 { // evenly divisible by 4 is
		return true
	}

	return false // otherwise is not
}
