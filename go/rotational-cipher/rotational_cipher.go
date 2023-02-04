package rotationalcipher

var latinLowercase []rune = []rune{
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
}
var latinUppercase []rune = []rune{
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
}

func RotationalCipher(input string, rotation int) string {
	if rotation == 0 || rotation == 26 {
		return input
	}

	var table []rune
	var origin, index rune
	rotated := make([]rune, 0, len(input))
	for _, c := range input {
		if c >= 'A' && c <= 'Z' {
			table = latinUppercase
			origin = 'A'
		} else if c >= 'a' && c <= 'z' {
			table = latinLowercase
			origin = 'a'
		} else {
			rotated = append(rotated, c)
			continue
		}
		index = c - origin + rune(rotation)
		if index > 25 {
			index -= 26
		}
		rotated = append(rotated, table[index])
	}

	return string(rotated)
}
