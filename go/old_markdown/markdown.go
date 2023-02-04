package markdown

// implementation to refactor

import (
	"fmt"
	"strings"
)

// Render translates markdown to HTML.
func Render(markdown string) string {
	return NewRender(markdown)
}

// NewRender is a refactoring of OldRender to convert markdown
// text to the equivalent HTML. The only supported tags in the
// original were headers (#), bold (__), italic (_), and
// unordered list (*). Alternate usages are not supported by
// the original and are not (yet) supported here.
func NewRender(markdown string) string {
	tokens := tokenize(strings.NewReader(markdown))
	w := &strings.Builder{}
	for _, t := range tokens {
		w.WriteString(t)
	}
	return w.String()
}

// Render translates markdown to HTML
func OldRender(markdown string) string {
	header := 0
	markdown = strings.Replace(markdown, "__", "<strong>", 1)
	markdown = strings.Replace(markdown, "__", "</strong>", 1)
	markdown = strings.Replace(markdown, "_", "<em>", 1)
	markdown = strings.Replace(markdown, "_", "</em>", 1)
	pos := 0
	list := 0
	html := ""
	for {
		char := markdown[pos]
		if char == '#' {
			for char == '#' {
				header++
				pos++
				char = markdown[pos]
			}
			html += fmt.Sprintf("<h%d>", header)
			pos++
			continue
		}
		if char == '*' {
			if list == 0 {
				html += "<ul>"
			}
			html += "<li>"
			list++
			pos += 2
			continue
		}
		if char == '\n' {
			if list > 0 {
				html += "</li>"
			}
			if header > 0 {
				html += fmt.Sprintf("</h%d>", header)
				header = 0
			}
			pos++
			continue
		}
		html += string(char)
		pos++
		if pos >= len(markdown) {
			break
		}
	}
	if header > 0 {
		return html + fmt.Sprintf("</h%d>", header)
	}
	if list > 0 {
		return html + "</li></ul>"
	}
	return "<p>" + html + "</p>"

}
