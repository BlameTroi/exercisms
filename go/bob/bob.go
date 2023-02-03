// a not so intelligent conversational responder named bob.
// bob is not eliza. his responses in conversation are lacking
// but do rely on correct punctuation.

package bob

import "strings"

// bob responds to tone (are you yelling) and inflection (was that
// a question) with the responses listed below.
//
// this program is US ASCII biased.
//
// i'd like to use a decision table for this, but i
// need to get all the conditions clearly defined
//
// regex is another option but it feels heavy for
// this task given the looseness of the spec.
// simplified specification:
//
// input always follows normal rules regarding sentence punctuation
// in English and yields one of the following 5 responses:
//
// question?	yelling?	response
// yes          no          Sure.
// yes          yes         Calm down, I know what I'm doing!
// no           no          Whatever.
// no           yes         Whoa, chill out!
//
// but: if nothing was said, respond "Fine. Be that way!".
// in any otyher case, respond "Whatever."

// character tests
func isWhitespace(c rune) bool {
	// ascii whitespace
	switch c {
	case ' ': // space
		return true
	case '\x09': // tab
		return true
	case '\x0a': // line feed
		return true
	case '\x0b': // vertical tab
		return true
	case '\x0c': // form feed
		return true
	case '\x0d': // carriage return
		return true
	}
	return false
}
func isLetter(c rune) bool {
	return 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z'
}
func isUppercase(c rune) bool {
	return 'A' <= c && c <= 'Z'
}
func isLowercase(c rune) bool {
	return 'a' <= c && c <= 'z'
}
func isDigit(c rune) bool {
	return '0' <= c && c <= '9'
}
func isPunctuation(c rune) bool {
	return c == '.' || c == '?' || c == '!' || c == ';' || c == ',' || c == ':'
}

// normalize the remark. remove non-essential characters,
// trim and pack whitespace, to get just the meaningful
// parts for this application.
func reduced(s string) string {
	// empty or blank strings are already reduced
	if len(strings.TrimSpace(s)) == 0 {
		return ""
	}
	// remove everything that isn't whitespace, punctuation, or
	// alphabetic
	s = strings.TrimSpace(s)
	r := ""
	last := ' '
	for _, curr := range s {
		// punctuation, alphabetic, and digits pass straight through
		if isLetter(curr) || isDigit(curr) || isPunctuation(curr) {
			last = curr
			r += string(curr)
			continue
		}
		// convert all whitespace to blanks
		if isWhitespace(curr) {
			curr = ' '
		}
		// reduce all runs whitespace to a single blank
		if curr != ' ' {
			continue
		}
		if last == curr {
			continue
		}
		last = curr
		r += string(curr)
	}
	// and we have an answer
	return r
}

// a question has at least one other character and ends with a question mark
func isQuestion(s string) bool {
	if len(s) < 2 {
		return false
	}
	return s[len(s)-1:] == "?"
}

// does the string contain any letters
func anyLetters(s string) bool {
	if len(s) == 0 {
		return false
	}
	return strings.ContainsAny(s, "abcdefghijklmnopqrstuvwxyz") ||
		strings.ContainsAny(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

// if the string has letters, are they call capitals
func allLettersCaps(s string) bool {
	if len(s) == 0 {
		return false
	}
	if !anyLetters(s) {
		return false
	}
	if strings.ContainsAny(s, "abcdefghijklmnopqrstuvwxyz") {
		return false
	}
	return true
}

// determine bob's response to the remark
func Hey(remark string) string {
	// clean up the remark for ease of processing
	remark = reduced(remark)

	if len(remark) == 0 {
		return "Fine. Be that way!"
	}
	if isQuestion(remark) {
		if allLettersCaps(remark) {
			return "Calm down, I know what I'm doing!"
		}
		return "Sure."
	}
	if allLettersCaps(remark) {
		return "Whoa, chill out!"
	}
	return "Whatever."
}
