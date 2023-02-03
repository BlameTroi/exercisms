package markdown

import (
	"fmt"
	"strings"
)

// The tokenize function parses Markdown formatted text
// into multiple tokens. Markdown directives become tokens
// and translated to html (eg., _ becomes <em>).
//
// Only a subset of Markdown is supported, and the subset is
// inferred from the original tests and original Render
// function.
//
// '__' brackets text to be rendered in bold as <strong>
// '_'  brackets text to be rendered in italics as <em>
// '#+' marks a header. The number of repeating # determines
//      the heading style <h1-?>
// '*'  marks an item in an unordered list. The first *
//      starts a new unordered list <ul> and <li>
func tokenize(r *strings.Reader) []string {
	tokens := []string{}
	w := &strings.Builder{}
	var inItalics, inBold bool
	var inList, inListItem, sawList bool
	var inHeader, sawHeader bool
	var headerLevel int
	var err error
	var c, ll rune
	c, ll = '\n', '\n'
	for {
		c, _, err = r.ReadRune()

		// any error is treated as eof. writing any trailing
		// markers or text will be handled after this loop
		if err != nil {
			break
		}

		// if collecting a header, collect to end of line
		if inHeader {
			if c != '\n' {
				w.WriteRune(c)
				continue
			}
			inHeader = false
			tokens = append(tokens, strings.TrimSpace(w.String()), fmt.Sprintf("</h%d>", headerLevel))
			w = &strings.Builder{}
			ll = c
			headerLevel = 0
			continue
		}

		// if collecting italics, watch for the end
		if inItalics {
			if c == '_' {
				inItalics = false
				if w.Len() != 0 {
					if len(tokens) > 0 && tokens[0] == "<li>" {
						tokens = append(tokens, strings.TrimSpace((w.String())))
					} else {
						tokens = append(tokens, w.String())
					}
					w = &strings.Builder{}
				}
				tokens = append(tokens, "</em>")
				continue
			}
		}

		// if collecting bold, watch for the end
		if inBold && c == '_' {
			rl, err := runLength(r)
			if err != nil {
				break
			}
			if rl == 2 {
				inBold = false
				if w.Len() != 0 {
					if len(tokens) > 0 && tokens[0] == "<li>" {
						tokens = append(tokens, strings.TrimSpace((w.String())))
					} else {
						tokens = append(tokens, w.String())
					}
					w = &strings.Builder{}
				}
				tokens = append(tokens, "</strong>")
				continue
			}
		}

		// if collecting list items, be sure to mark beginning
		// and end
		if inListItem && c == '\n' {
			rl, err := runLength(r)
			if err != nil {
				break
			}
			if rl > 1 {
				inListItem = false
				inList = false
				tokens = append(tokens, strings.TrimSpace(w.String()), "</li></ul>")
				w = &strings.Builder{}
				continue
			}
			tokens = append(tokens, strings.TrimSpace(w.String()), "</li>")
			w = &strings.Builder{}
			inListItem = false
			continue
		}

		// starting italics or bold?
		if c == '_' {

			if w.Len() != 0 {
				if len(tokens) > 0 && tokens[0] == "<li>" {
					tokens = append(tokens, strings.TrimSpace((w.String())))
				} else {
					tokens = append(tokens, w.String())
				}
				w = &strings.Builder{}
			}

			rl, err := runLength(r)
			// should i do more than continue on error?
			if err != nil {
				continue
			}

			if rl == 3 {
				inBold = true
				inItalics = true
				tokens = append(tokens, "<em><strong>")
				continue
			}

			if rl == 2 {
				inBold = true
				tokens = append(tokens, "<strong>")
				continue

			}
			if rl == 1 {
				inItalics = true
				tokens = append(tokens, "<em>")
				continue
			}

			// you can bold, italicize, or both, but
			// no further
			w.WriteString(strings.Repeat("_", rl))
			continue
		}

		// start of header? this should only valid if the preceeding
		// non whitespace character was a newline (&& lh == '\n')
		// but the original implementation allows leading.
		if c == '#' {
			headerLevel, err = runLength(r)
			if err != nil {
				break
			}
			tokens = append(tokens, w.String(), fmt.Sprintf("<h%d>", headerLevel))
			inHeader = true
			sawHeader = true
			continue
		}

		// start of list or list item? we only acknowledge the *
		// form not the - form.
		if c == '*' {
			if !inList {
				tokens = append(tokens, w.String(), "<ul>")
				inList = true
				sawList = true
			}
			inListItem = true
			tokens = append(tokens, w.String(), "<li>")
			continue
		}

		// no marker, just pass the character through
		w.WriteRune(c)

		// remember if the last character was a newline ...
		// note that trailing whitespace after a newline are
		// newlines for parsing lists above
		if ll == '\n' && c != ' ' && c != '\t' && c != '\n' {
			ll = c
		}
	}

	// flush bolded if we were building one
	if inBold {
		if w.Len() != 0 {
			tokens = append(tokens, w.String())
			w = &strings.Builder{}
		}
		tokens = append(tokens, "</strong>")
		inBold = false
	}

	// flush italicized if we were building one
	if inItalics {
		if w.Len() != 0 {
			tokens = append(tokens, w.String())
			w = &strings.Builder{}
		}
		tokens = append(tokens, "</em>")
		inItalics = false
	}

	// flush list if we were building one
	if inList {
		tokens = append(tokens, strings.TrimSpace(w.String()), "</li></ul>")
		w = &strings.Builder{}
		inList = false
	}

	// flush header if we were building one
	if inHeader {
		if w.Len() != 0 {
			tokens = append(tokens, strings.TrimSpace(w.String()))
			w = &strings.Builder{}
		}
		tokens = append(tokens, fmt.Sprintf("</h%d>", headerLevel))
		inHeader = false
	}

	// if there is anything still in buffer, add it to
	// output
	if w.Len() != 0 {
		tokens = append(tokens, w.String())
	}

	// if there was nothing in the input, leave one empty
	// string in the result
	if len(tokens) == 0 {
		tokens = append(tokens, "<p></p>")
	} else if !sawHeader && !sawList {
		temp := []string{"<p>"}
		temp = append(temp, tokens...)
		tokens = append(temp, "</p>")
	}

	return tokens
}

func runLength(r *strings.Reader) (int, error) {
	r.UnreadRune()
	i := 1
	c, _, err := r.ReadRune()
	if err != nil {
		return 0, err
	}
	for {
		n, _, err := r.ReadRune()
		if err != nil {
			return i, err
		}
		if n != c {
			r.UnreadRune()
			return i, nil
		}
		i++
	}
}
