// Package protein provides simple mapping of RNA to
// proteins.
package protein

import (
	"errors"
	"io"
	"strings"
)

// getCodonMap returns a fresh copy of the codon to
// protein map.
//
// Multiple codons can map to the same protein. A
// codon can signal STOP, which means that translation
// of the RNA is complete and no following codons
// should be considered.
func getCodonMap() map[string]string {
	return map[string]string{
		"AUG": "Methionine",
		"UUU": "Phenylalanine",
		"UUC": "Phenylalanine",
		"UUA": "Leucine",
		"UUG": "Leucine",
		"UCU": "Serine",
		"UCC": "Serine",
		"UCA": "Serine",
		"UCG": "Serine",
		"UAU": "Tyrosine",
		"UAC": "Tyrosine",
		"UGU": "Cysteine",
		"UGC": "Cysteine",
		"UGG": "Tryptophan",
		"UAA": "STOP",
		"UAG": "STOP",
		"UGA": "STOP",
	}
}

// ErrInvalidBase is returned when an unknown codon is
// encountered.
var ErrInvalidBase = errors.New("invalid codon base")

// ErrStop is returned when a STOP codon is encountered.
var ErrStop = errors.New("STOP codon found")

// FromCodon takes a single codon and returns its
// protien name. Errors ErrInvalidBase and ErrStop
// can be returned.
func FromCodon(c string) (string, error) {
	cm := getCodonMap()
	p, exists := cm[c]
	if !exists {
		return "", ErrInvalidBase
	}
	if p == "STOP" {
		return "", ErrStop
	}
	return p, nil
}

// nextCodon assembles a codon (sequence of three
// characters) from a Reader. EOF is expected on
// the first ReadRune. Regardless, the error is
// returned to the caller.
func nextCodon(r *strings.Reader) (string, error) {
	var c []rune
	for i := 0; i < 3; i++ {
		ch, _, err := r.ReadRune()
		if err != nil {
			return "", err
		}
		c = append(c, ch)
	}
	return string(c), nil
}

// FromRNA translates an RNA string into a list (array)
// of protein names. Translation ends cleanly upon
// reaching the end of the string or when a STOP codon
// is found. If an invalid codon is found, any proteins
// decoded so far are returned along with ErrInvalidBase.
func FromRNA(r string) ([]string, error) {
	cm := getCodonMap()
	rr := strings.NewReader(r)
	ps := []string{}
	var c string
	var err error
	for {
		c, err = nextCodon(rr)
		if err == io.EOF {
			return ps, nil
		}
		if err == ErrInvalidBase {
			// spec wants translation so far and then error return
			return ps, ErrInvalidBase
		}
		pm, exists := cm[c]
		if !exists {
			// spec wants translation so far and then error return
			return ps, ErrInvalidBase
		}
		if pm == "STOP" {
			return ps, nil
		}
		ps = append(ps, pm)
	}
}
