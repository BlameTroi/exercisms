// Package markdown provides a very limited markdown to html rendering.
package markdown

import (
	"fmt"
	"strings"
)

// TokenRender is a refactoring of OldRender to convert markdown
// text to the equivalent HTML. The only supported tags in the
// original were headers (#), bold (__), italic (_), and
// unordered list (*). Alternate usages are not supported by
// the original and are not (yet) supported here.
//
// The approach for this pass is to do a traditional scan
// and tokenize. The tokenizer breaks the input stream into
// text and markup, and translates the markup from markdown
// syntax to html tags.
//
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
//
// The reference implementation wraps the rendered HTML
// in <p></p> unless a list or header is encountered. We
// assume that the paragraph markers are needed and remove
// them at the end if they are not.
func TokenRender(md string) string {
	r := strings.NewReader(md)
	closers := stack{}
	tokens := []string{"<p>"}
	closers.push("</p>")
	var sawList, sawHeader bool
	var inList, inListItem bool
	w := &strings.Builder{}
	for {
		c, _, err := r.ReadRune()
		if err != nil {
			break
		}

		if c == '\n' && !closers.empty() && closers.peek()[2] == 'h' {
			if w.Len() != 0 {
				tokens = append(tokens, w.String())
				w = &strings.Builder{}
			}
			tokens = append(tokens, closers.pop())
			continue
		}

		if c == '\n' && inListItem {
			if w.Len() != 0 {
				tokens = append(tokens, w.String())
				w = &strings.Builder{}
			}
			for closers.peek() != "</li>" {
				tokens = append(tokens, closers.pop())
			}
			tokens = append(tokens, closers.pop())
			inListItem = false
			continue
		}

		if !isMarkdownRune(c) {
			w.WriteRune(c)
			continue
		}

		if w.Len() != 0 {
			tokens = append(tokens, w.String())
			w = &strings.Builder{}
		}

		if c == '*' {
			sawList = true
			if !inList {
				inList = true
				tokens = append(tokens, "<ul>")
				closers.push("</ul>")
			}
			tokens = append(tokens, "<li>")
			closers.push("</li>")
			skipBlanks(r)
			inListItem = true
			continue
		}

		rl, _ := runLength(r)

		if c == '#' {
			sawHeader = true
			tokens = append(tokens, fmt.Sprintf("<h%d>", rl))
			closers.push(fmt.Sprintf("</h%d>", rl))
			skipBlanks(r)
			continue
		}

		if c != '_' {
			// this should not occur
			continue
		}

		// it is posible to concatenate bold and italic, as in
		// ___ -> <strong><em>. for any run of multiple _ chars,
		// consume two at a time as bold, and if there is an
		// odd one left, add an italic
		for rl > 1 {
			if !closers.empty() && closers.peek() == "</strong>" {
				tokens = append(tokens, closers.pop())
			} else {
				closers.push("</strong>")
				tokens = append(tokens, "<strong>")
			}
			rl -= 2
		}

		if rl == 1 {
			if !closers.empty() && closers.peek() == "</em>" {
				tokens = append(tokens, closers.pop())
			} else {
				closers.push("</em>")
				tokens = append(tokens, "<em>")
			}
			continue
		}

		// this should never be reached
	}

	// if there's any text buffered, add it.
	if w.Len() != 0 {
		tokens = append(tokens, w.String())
	}

	// add any as unprocessed closing tags. end of file
	// logically closes anything that is still open.
	for !closers.empty() {
		tokens = append(tokens, closers.pop())
	}

	// remove the wrapping paragraph tags if they are
	// there and a list or header was seen.
	s, e := 0, len(tokens)
	if (sawHeader || sawList) && tokens[0] == "<p>" {
		s, e = 1, e-1
	}

	w = &strings.Builder{}
	for i := s; i < e; i++ {
		w.WriteString(tokens[i])
	}

	return w.String()
}
