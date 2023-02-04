// Package armstrong determines if a number is an Armstrong number.
package armstrong

// Using strconv.Itoa is one way to do this, but where's the fun
// in that?

// IsNumber tests a given number for Armstrongness.
func IsNumber(n int) bool {
	// normalize
	if n < 0 {
		n = -n
	}

	// find starting power of 10 for
	// digitizing
	power, prior, count := 1, 1, 1
	for power <= n {
		prior = power
		power *= 10
		count++
	}
	power = prior

	// extract digits, from the most to the least significant.
	digits := make([]int, 0, count)
	temp := n
	for power > 1 {
		d := temp / power
		temp %= power
		power /= 10
		digits = append(digits, d)
	}
	digits = append(digits, temp)

	// armstrong numbers are those that is the sum of its
	// digits each raised to the power of the number of
	// digits
	fraise := func(b, p int) int {
		if p == 0 {
			return 1
		}
		n := b
		for r := 1; r < p; r++ {
			n *= b
		}
		return n
	}
	sum := 0
	for _, d := range digits {
		sum += fraise(d, len(digits))
	}

	return sum == n
}
