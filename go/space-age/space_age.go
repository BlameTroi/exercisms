// how old would someone be on a specific planet in the
// sol system given an age in seconds.
package space

type Planet string

// create a structure mapping planet names to the
// length of the planets year in seconds given:
//
// Mercury: orbital period 0.2408467 Earth years
// Venus: orbital period 0.61519726 Earth years
// Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
// Mars: orbital period 1.8808158 Earth years
// Jupiter: orbital period 11.862615 Earth years
// Saturn: orbital period 29.447498 Earth years
// Uranus: orbital period 84.016846 Earth years
// Neptune: orbital period 164.79132 Earth years

const earthYear = 31557600

type orbitalPeriods struct {
	planet  Planet
	seconds float64
}

var gustavHolst = []orbitalPeriods{
	{"Mercury", 0.24084667 * earthYear},
	{"Venus", 0.61519726 * earthYear},
	{"Earth", earthYear},
	{"Mars", 1.8808158 * earthYear},
	{"Jupiter", 11.862615 * earthYear},
	{"Saturn", 29.447498 * earthYear},
	{"Uranus", 84.016846 * earthYear},
	{"Neptune", 164.79132 * earthYear},
}

// return the age on planet p.
// the spec does not provide direction on what
// to do if an invalid planet name is passed
// but to avoid the hole in the bucket syndrome
// i was returning 0.0. the tests were just
// updated and a -1.0 is now expected on
// error.
func Age(s float64, p Planet) float64 {
	for _, gh := range gustavHolst {
		if gh.planet == p {
			return s / gh.seconds
		}
	}
	return -1.0
}
