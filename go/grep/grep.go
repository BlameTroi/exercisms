// Package grep provides a less functional version of the
// actual *nix greps.
package grep

import (
	"bufio"
	"os"
	"regexp"
	"strconv"
)

// Search runs a grep style search on a set of files. Flags are
// command line style flags but the flags package doesn't
// currently support reading flags from arbitrary strings.
func Search(pattern string, flags, files []string) []string {
	res := []string{}
	c := &grepContext{}

	c.buildContext(pattern, flags)
	if len(files) > 1 {
		c.printFileName = true
	}

	for _, fn := range files {
		res = append(res, c.scanFile(fn)...)
	}

	return res
}

// Context stores flags, search string, and the compiled regular
// expression. Various subfunctions of Search are called as methods
// off this grepContext.
//
// While more plumbing would be needed, using this context would
// allow each file scan to run concurrently.
//
// flags
// -n Print the line numbers of each matching line.
// -l Print only the names of files that contain at least one matching line.
// -i Match line using a case-insensitive comparison.
// -v Invert the program -- collect all lines that fail to match the pattern.
// -x Only match entire lines, instead of lines that contain a match.
type grepContext struct {
	regex              *regexp.Regexp
	printLineNumbers   bool
	onlyPrintFileNames bool
	caseInsensitive    bool
	inverted           bool
	matchCompleteLine  bool
	printFileName      bool
}

// Parse runtime flags and create a regular expression from the
// supplied pattern string with any modifications required by
// the flags.
func (c *grepContext) buildContext(pattern string, flags []string) {

	for _, f := range flags {
		switch f {
		case "-n":
			c.printLineNumbers = true
		case "-l":
			c.onlyPrintFileNames = true
		case "-i":
			c.caseInsensitive = true
		case "-v":
			c.inverted = true
		case "-x":
			c.matchCompleteLine = true
		}
	}

	if c.matchCompleteLine {
		pattern = "^" + pattern + "$"
	}
	if c.caseInsensitive {
		pattern = "(?i)" + pattern
	}
	c.regex = regexp.MustCompile(pattern)
}

// scanFile reads the file line by line and attempts to match
// each line against the supplied regular expresion. Matches
// (or non-matches if running -v) are returned as a slice of
// strings. Depending on runtime flags, each string can include
// a file name, line number, and the text of the line.
func (c grepContext) scanFile(fn string) []string {
	res := []string{}

	f, err := os.Open(fn)
	if err != nil {
		return res
	}
	defer f.Close()

	s := bufio.NewScanner(f)
	n := 0

	for s.Scan() {
		n++
		l := s.Text()
		r := ""
		m := c.regex.MatchString(l)
		if c.inverted {
			m = !m
		}
		if m {
			if c.onlyPrintFileNames {
				res = append(res, fn)
				break
			}
			if c.printFileName {
				r = r + fn + ":"
			}
			if c.printLineNumbers {
				r = r + strconv.Itoa(n) + ":"
			}
			r = r + l
			res = append(res, r)
		}
	}

	return res
}
