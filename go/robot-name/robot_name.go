// Package robotname provides a unique name of a specific pattern.
// The name generation should not be deterministic. Even though a
// robot can be reset and its name cleared, a name may not be
// reused.
package robotname

import (
	"crypto/rand"
	"errors"
	"fmt"
	"math/big"
)

// Robot state. Access is via methods.
type Robot struct {
	name string
}

// Name returns the unique name for a particular Robot
// via lazy initialization. If all available names
// have been used, return an error.
func (r *Robot) Name() (string, error) {
	if r.name != "" {
		return r.name, nil
	}
	if len(used) == namespaceCap {
		return "", errors.New("all names already used")
	}
	r.name = candidate()
	_, exists := used[r.name]
	for exists {
		r.name = candidate()
		_, exists = used[r.name]
	}
	used[r.name] = 1
	return r.name, nil
}

// Reset a Robot to factory default. Its current name is
// cleared and if it is reactivated, it will be assigned
// a new name. Its old name can not be reused.
func (r *Robot) Reset() {
	r.name = ""
}

// Robot names used store, and the maximum size of the
// space for names.
var used map[string]byte
var namespaceCap = 26 * 26 * 10 * 10 * 10

// A simple but hopefully sufficiently random candidate name
// generator using crypto/rand instead of math/rand.
var bi26 *big.Int
var bi10 *big.Int

func init() {
	used = map[string]byte{}
	bi26 = big.NewInt(26)
	bi10 = big.NewInt(10)
}

func randomChar(start rune, end *big.Int) rune {
	i, err := rand.Int(rand.Reader, end)
	if err != nil {
		panic(fmt.Errorf("unrecoverable error in crypto/rand: %w", err))
	}
	return start + rune(i.Int64())
}

func letter() rune {
	return randomChar('A', bi26)
}

func number() rune {
	return randomChar('0', bi10)
}

func candidate() string {
	return string([]rune{letter(), letter(), number(), number(), number()})
}
