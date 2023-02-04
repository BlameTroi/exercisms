package perfect

import "errors"

type Classification int

const (
	ClassificationAbundant  = 1
	ClassificationDeficient = 2
	ClassificationPerfect   = 3
)

var ErrOnlyPositive = errors.New("number must be positive")

func Classify(n int64) (Classification, error) {
	if n < 1 {
		return 0, ErrOnlyPositive
	}
	if n == 1 {
		return ClassificationDeficient, nil
	}
	as := int64(0)
	lim := n / 2
	for i := int64(1); i <= lim; i++ {
		if n%i == 0 {
			as += i
		}
	}
	if as < n {
		return ClassificationDeficient, nil
	}
	if as > n {
		return ClassificationAbundant, nil
	}
	return ClassificationPerfect, nil
}
