package sieve

// Sieve returns all primes up to and including the
// limit (max) passed, and returns the primes as a
// slice of ints.
//
// todo: larger, unsigned, and bit-mapped
func Sieve(lim int) []int {
	primes := make([]int, 0, lim/2)
	sieve := make([]bool, lim+1)

	if lim < 2 {
		return []int{}
	}

	for i := 2; i <= lim; i++ {
		sieve[i] = true
	}
	sieve[0] = false
	sieve[1] = false

	for i := 2; i <= lim; i++ {
		if sieve[i] {
			primes = append(primes, i)
			for j := i + i; j <= lim; j += i {
				sieve[j] = false
			}
		}
	}

	return primes
}
