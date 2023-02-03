// add a gigasecond to a time value.
package gigasecond

import "time"

// A gigasecond is 10^9 (1,000,000,000) seconds.
const gigasec = time.Second * 1000000000

// given time t, add a gigasecond to that time and return
// the new time.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(gigasec)
}
