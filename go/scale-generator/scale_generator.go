package scale

import "strings"

// Given a tonic, or starting note, and a set of intervals, generate
// the musical scale starting with the tonic and following the
// specified interval pattern.
//
// A, A#, B, C, C#, D, D#, E, F, F#, G, G#
//
// A, Bb, B, C, Db, D, Eb, E, F, Gb, G, Ab
//
// m half step -- distance between adjacent notes, A to A# (or A to Bb) -- minor second
// M whole step -- intervening note, A to B skipping A#/Bb -- major second
//
//    tonic = C
// interval = null
//   result = C, C#, D, D#, E, F, F#, G, G#, A, A#, B
//
//    tonic = C
// interval = MMmMMMm
//   result = C, D, E, F, G, A, B
//
// A . B C . D . E F . G .
//
// "." can be either beneath-# or above-b
//
// case insensitive for tonic
// upper case out for result
//

var notes map[int]string

// var steps []string
// var sharp string
// var flat string
var full string

func init() {
	notes := make(map[int]string)
	notes[1] = "A"
	notes[2] = "."
	notes[3] = "B"
	notes[4] = "C"
	notes[5] = "."
	notes[6] = "D"
	notes[7] = "."
	notes[8] = "E"
	notes[9] = "F"
	notes[10] = "."
	notes[11] = "G"
	notes[12] = "."
	// steps = []string{"m", "M"}
	// sharp = "#"
	// flat = "b"
	full = strings.Repeat("m", 12)
}

func MyScale(tonic, interval string) []string {
	result := []string{}
	if interval == "" {
		interval = full
	}
	currkey := 0
	// assume simple case, tonic is neither sharp nor flat
	for k, v := range notes {
		if v == tonic {
			currkey = k
		}
	}
	// spec does not allow for error?
	for range interval {
		result = append(result, notes[currkey])
		currkey++
		if currkey > 12 {
			currkey = 1
		}
	}

	// todo: work only in upper case for notes

	// find start in scale

	return result
}

//
// a working solution to help figure out the spec
//
// Package scale provides functions for musical scale generation.

var scalesSharp = []string{"A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"}
var scalesFlat = []string{"A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"}
var diatonicFlats = []string{"F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb"}
var intervalStep = map[rune]int{'m': 1, 'M': 2, 'A': 3}

// Scale generate musical scale.
func Scale(tonic, interval string) []string {
	var scalesCurrent []string
	var scalesResult []string
	if indexOf(tonic, diatonicFlats) != -1 {
		scalesCurrent = scalesFlat
	} else {
		scalesCurrent = scalesSharp
	}
	if interval == "" {
		interval = "mmmmmmmmmmmm"
	}
	index := indexOf(strings.Title(tonic), scalesCurrent)
	for _, val := range interval {
		scalesResult = append(scalesResult, scalesCurrent[index])
		index += intervalStep[val]
		if index >= len(scalesCurrent) {
			index -= len(scalesCurrent)
		}
	}
	return scalesResult
}
func indexOf(value string, strigs []string) int {
	for i, v := range strigs {
		if v == value {
			return i
		}
	}
	return -1
}
