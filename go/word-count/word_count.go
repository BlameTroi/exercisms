package wordcount

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
	numbers    runeList = runeList{
		'0', '1', '2', '3', '4',
		'5', '6', '7', '8', '9'}
	alphabetic runeList = runeList{
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
		'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
		'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
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
		if len(word) == 0 && (r == '\'' || r == '"') {
			// don't put leading quotes on a word
			continue
		}
		if breakers.contains(r) {
			if len(word) == 0 {
				// contiguous break characters
				continue
			}
			for len(word) > 0 && (word[len(word)-1] == '\'' || word[len(word)-1] == '"') {
				// trim any trailing quotes. this could produce an empty word.
				word = word[:len(word)-1]
			}
			if len(word) == 0 {
				continue
			}
			if word.only(numbers) {
				words[string(word)] += 1
			} else if word.only(alphabetic) {
				words[string(word.tolower())] += 1
			}
			word = make(runeList, 0, 8)
			continue
		}
		word = append(word, r)
	}
	return words
}
