// Package markdown provides a very limited markdown to html rendering.
package markdown

import (
	"bufio"
	"fmt"
	"regexp"
	"strings"
)

// RegexRender is a refactoring of OldRender to convert markdown
// text to the equivalent HTML. The only supported tags in the
// original were headers (#), bold (__), italic (_), and
// unordered list (*). Alternate usages are not supported by
// the original and are not (yet) supported here.
//
// The approach for this pass is to use regular expressions to
// identify markdown directives and replace them with the proper
// html tags.
//
// The token based approach in TokenRender runs in about half the
// time and uses less memory than the OriginalRender. It will be
// interesting to compare this approach with TokenRender. I will
// not be surprised at either outcome.
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
// in <p></p> unless a list or header is encountered.
func RegexRender(md string) string {

	//	s := bufio.NewScanner(strings.NewReader(md))
	//  w := strings.Builder{}

	reItalic := regexp.MustCompile(`(?U)_([^_].+)_`)
	reBold := regexp.MustCompile(`(?U)__(.+)__`)
	reListItem := regexp.MustCompile(`(?Um)^\s*\* (.+)$`)
	reHeader := regexp.MustCompile(`(?Um)^(\s*#+) (.+)$`)

	var wrapIt bool
	var hasHeader, hasList bool
	hasHeader = reHeader.MatchString(md)
	hasList = reListItem.MatchString(md)
	wrapIt = !hasHeader && !hasList
	var hasTrailingNewline = strings.HasSuffix(md, "\n")

	// bold and italic do not respect newlines, so work on the whole blob
	md = reBold.ReplaceAllString(md, "<strong>$1</strong>")
	md = reItalic.ReplaceAllString(md, "<em>$1</em>")

	// headers and lists respect newlines, so this part has to be done line by line

	// headers
	if hasHeader {
		s := bufio.NewScanner(strings.NewReader(md))
		w := strings.Builder{}
		for s.Scan() {
			t := s.Text()
			match := reHeader.FindAllStringSubmatch(t, 1)
			if len(match) != 0 {
				hn := len(strings.TrimSpace(match[0][1]))
				hs := fmt.Sprintf("<h%d>$2</h%d>", hn, hn)
				t = reHeader.ReplaceAllString(t, hs)
			}
			w.WriteString(t)
			w.WriteRune('\n')
		}
		// trim off trailing \n if we added one that shouldn't be there
		md = w.String()
		if !hasTrailingNewline {
			md = strings.TrimSuffix(md, "\n")
		}
	}

	// lists
	var inList bool
	if hasList {
		s := bufio.NewScanner(strings.NewReader(md))
		w := strings.Builder{}
		for s.Scan() {
			t := s.Text()
			match := reListItem.FindAllStringSubmatch(t, 1)
			if len(match) != 0 {
				if !inList {
					inList = true
					w.WriteString("<ul>")
				}
				t = reListItem.ReplaceAllString(t, "<li>$1</li>")
			} else {
				if inList {
					inList = false
					w.WriteString("</ul>")
				}
			}
			w.WriteString(t)
		}
		if inList {
			w.WriteString("</ul>")
		}
		// restore trailing \n if there was one
		if hasTrailingNewline {
			w.WriteRune('\n')
		}
		md = w.String()
	}

	if wrapIt {
		md = "<p>" + md + "</p>"
	}
	// if hasList {
	// 	md = md + html.EscapeString("\n***need to add <ul></ul>")
	// }

	return md
}
