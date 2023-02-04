// Package dna provides various functions to analyze DNA nucleotide
// strings.
package dna

import "errors"

// Histogram is a mapping from nucleotide to its count in a DNA
// string. 
type Histogram map[rune]int

// DNA is a list of nucleotides in a string. 
type DNA string

// Counts generates a histogram of valid nucleotides in the given DNA.
// Returns an error if the dna contains an invalid nucleotide code.
// valid values are A, C, G, and T.
func (d DNA) Counts() (Histogram, error) {
	h := Histogram{'A': 0, 'C': 0, 'G': 0, 'T': 0}
	if len(d) == 0 {
		return h, nil
	}
	for _, r := range d {
		if _, ok := h[r]; !ok {
			return nil, errors.New("invalid nucleotide in DNA string: " + string(r))
		}
		h[r]++
	}
	return h, nil
}
