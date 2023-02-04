// Package clock provides an hours-and-minutes 24 hour clock
// style time.
package clock

import "fmt"

// Clock is a 24 hour clock with a resolution of minutes. No
// internal state accessors are exported. Constructor and
// stringer interfaces are provided, as is manipulation in
// the form of adding or subtracting a number of minutes.
type Clock struct {
	minutes int
}

// New takes hour and minute values (which may be negative and/or
// outside the traditional bounds of 0-24 and 0-59) and returns a
// Clock instance of the equivalent traditional time; the time is
// normalized or reduced to fit in a 24 hour clock.
func New(hour, minute int) Clock {
	return Clock{((hour%24)*60 + minute) % 1440}.normalized()
}

// String is a stringer for Clock.
func (c Clock) String() string {
	h, m := c.minutes/60, c.minutes%60
	return fmt.Sprintf("%02d:%02d", h, m)
}

// Add minutes to the Clock, returning a new Clock.
func (c Clock) Add(minutes int) Clock {
	return Clock{(c.minutes + minutes) % 1440}.normalized()
}

// Subtract minutes from the Clock, returning a new Clock.
func (c Clock) Subtract(minutes int) Clock {
	return Clock{(c.minutes - minutes) % 1440}.normalized()
}

// Normalize the Clock to fit in the 00:00 -> 23:59 window.
// A Clock could be negative, in which case it refers to the
// "prior" day, so -00:15 becomes 23:45.
func (c Clock) normalized() Clock {
	m := c.minutes % 1440
	if m < 0 {
		m = 1440 + m
	}
	return Clock{m}
}
