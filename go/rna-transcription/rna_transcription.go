// The strand package converts DNA strands to
// their RNA complement.
package strand

import "strings"

// ToRNA takes a string representation of a DNA strand
// and returns its RNA complement (via transcription).
//
// No validation is performed. If an invalid nucleotide
// is in the DNA string it is replaced with a "?".
func ToRNA(dna string) string {
	var rna strings.Builder
	for _, nuc := range dna {
		switch nuc {
		case 'G':
			rna.WriteRune('C')
		case 'C':
			rna.WriteRune('G')
		case 'T':
			rna.WriteRune('A')
		case 'A':
			rna.WriteRune('U')
		default:
			rna.WriteRune('?')
		}
	}
	return rna.String()
}
