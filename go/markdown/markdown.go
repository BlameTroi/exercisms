// Package markdown provides a very limited markdown to html rendering.
package markdown

// The assignment is to refactor the provided implementation into
// something more reliable, more readable, and easier to extend.
//
// The obvious approach is to use regexp, but I started with a more
// traditional tokenizer.
//
// First, I created a new Render function that calls the appropriate
// implementation. The original implementation is named with the
// obvious OriginalRender. TokenRender and RegexRender reside in
// separate source files.
//
// The tests provided by exercism were pretty limited, and the 'loop
// through all the cases' approach is not TDD friendly. I kept those
// tests as acceptance tests and created parallel tests for both
// new Renders as I worked through each. These are included in this
// iteration.
//
// After passing all tests, I did some benchmarks using the tests
// provided by exercism. The results show that regexp can be slow, but
// that less code (original) does not always lead to faster code (token).
//
// goos: windows
// goarch: amd64
// pkg: blametroi/markdown
// cpu: Intel(R) Core(TM) i7-4790K CPU @ 4.00GHz
// BenchmarkMarkdown_Original-8       71127             16458 ns/op            8963 B/op        531 allocs/op
// BenchmarkMarkdown_Token-8         140232              8816 ns/op            5530 B/op        128 allocs/op
// BenchmarkMarkdown_Regex-8           7072            144253 ns/op          134611 B/op       1148 allocs/op
// PASS
// ok      blametroi/markdown   3.782s
//
// Running these against real world markdown (a 24k readme.md from github) got
// a panic in the original version, but completed with not too much garbling
// in the other implementations.

import (
	"fmt"
	"strings"
)

// Render translates markdown to HTML. Uncomment the desired
// style of rendering. This is the entry point that the
// exercism site tests will use.
func Render(markdown string) string {
	// html := OldRender(markdown)
	html := TokenRender(markdown)
	// html := RegexRender(markdown)
	return html
}

// Render translates markdown to HTML. This is the original
// version to be refactored.
func OriginalRender(markdown string) string {
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
