// Package scrabble provides scoring for words in
// the game of Scrabble.
package scrabble

import "strings"

// Function getLetterScores returns a new array of
// strings. The array index is the score for a
// letter in Scrabble, and the string holds all of
// the letters that have that score value. Empty
// entries are used for spacing.
func getLetterScores() []string {
	return []string{
		"",           // 0
		"AEIOULNRST", // 1
		"DG",         // 2
		"BCMP",       // 3
		"FHVWY",      // 4
		"K",          // 5
		"",           // 6
		"",           // 7
		"JX",         // 8
		"",           // 9
		"QZ",         // 10
	}
}

// Score takes a string holding a word and
// returns the base score of that word in
// Scrabble.
func Score(word string) int {
	// Guard clauses.
	if len(word) == 0 {
		return 0
	}
	// Tally the tiles.
	letterScores := getLetterScores()
	total := 0
	for _, letter := range strings.ToUpper(word) {
		for score, letters := range letterScores {
			if strings.IndexRune(letters, letter) != -1 {
				total += score
				break
			}
		}
	}
	return total
}
