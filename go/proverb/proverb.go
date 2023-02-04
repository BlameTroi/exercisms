// Package proverb creates a rhyme after the pattern "For want
// of a nail the shoe was lost..."
package proverb

// Proverb takes an array of strings (typically nouns) and builds
// an array of lines holding the resulting rhyme. If no rhyme can
// be formed an empty array is returned.
//
// At least one noun is required; to be interesting at least three
// should be provided.
//
// Given {A, B, C} Proverb returns
//
// For want of a A the B was lost.
// For want of a B the C was lost.
// All for the want of an A.
func Proverb(nouns []string) []string {

	if len(nouns) < 1 {
		return []string{}
	}

	// Assemble the rhyme from the nouns.
	rhyme := []string{}
	for i := 0; i < len(nouns)-1; i++ {
		rhyme = append(rhyme, "For want of a "+nouns[i]+" the "+nouns[i+1]+" was lost.")
	}

	// Conclude the rhyme with a summary reference to the first
	// item and return.
	rhyme = append(rhyme, "And all for the want of a "+nouns[0]+".")

	return rhyme
}
