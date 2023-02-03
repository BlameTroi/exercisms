package wordcount

//
// func WordCount(phrase string) Frequency  // Implement this function.
// type Frequency map[string]int            // Using this return type.

type Frequency map[string]int
type runeList []rune

var (
	breakers runeList = runeList{
		' ', '\n', '\t',
		',', ':', '!', '.', '?',
		'&', '%', '@', '^', '*', '/', '$',
		'(', ')', '[', ']', '{', '}', '<', '>',
		'"', '\\',
	}
	numbers    runeList = runeList{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
	alphabetic runeList = runeList{
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
		'\'',
	}
)

func (rl runeList) contains(r rune) bool {
	for i := 0; i < len(rl); i++ {
		if r == rl[i] {
			return true
		}
	}
	return false
}

func (rl runeList) only(allowed runeList) bool {
	for _, r := range rl {
		if !allowed.contains(r) {
			return false
		}
	}
	return true
}

func (rl runeList) tolower() runeList {
	lowered := make(runeList, len(rl))
	for i, r := range rl {
		lowered[i] = lowercase(r)
	}
	return lowered
}

func (rl runeList) unquote() runeList {
	if len(rl) < 3 {
		return rl
	}
	if rl[0] != '\'' && rl[0] != '"' {
		return rl
	}
	if rl[len(rl)-1] == rl[0] {
		return rl[1 : len(rl)-1]
	}
	return rl
}

func lowercase(r rune) rune {
	if r >= 'A' && r <= 'Z' {
		return r + 'a' - 'A'
	}
	return r
}

func WordCount(phrase string) Frequency {
	words := make(Frequency)
	var word runeList = make(runeList, 0, 8)
	for _, r := range phrase + " " {
		if breakers.contains(r) {
			if len(word) != 0 {
				if word.only(numbers) {
					words[string(word)] += 1
				} else if word.only(alphabetic) {

					words[string(word.tolower().unquote())] += 1
				}
				word = make(runeList, 0, 8)
			}
			continue
		}
		word = append(word, r)
	}
	return words
}
