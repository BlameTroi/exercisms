// Package markdown provides a very limited markdown to html rendering.
package markdown

import (
	"testing"
)

// These tests are ones written to drive me through
// debugging during the refactoring. The combined tests in
// markkdown_test.go and cases_test.go are the acceptance
// tests for the final implementation as provided by exercism.
//
// The tests that exercism provides are not written for TDD,
// so for this problem I've decided to use individual tests
// to help work through the code.

func Test_Regex_Empty(t *testing.T) {

	assertRender(t, RegexRender, "", "<p></p>")

}

func Test_Regex_Paragraph(t *testing.T) {

	input := "This will become a paragraph."
	want := "<p>This will become a paragraph.</p>"
	assertRender(t, RegexRender, input, want)

	input = "This will become\na paragraph."
	want = "<p>This will become\na paragraph.</p>"
	assertRender(t, RegexRender, input, want)

}

func Test_Regex_Italics(t *testing.T) {

	input := "_This will be italic._"
	want := "<p><em>" + "This will be italic." + "</em></p>"
	assertRender(t, RegexRender, input, want)

	input = "_This_ starts and ends with _italics_."
	want = "<p><em>This</em> starts and ends with <em>italics</em>.</p>"
	assertRender(t, RegexRender, input, want)

}

func Test_Regex_Bold(t *testing.T) {

	input := "__This will be bolded.__"
	want := "<p><strong>This will be bolded.</strong></p>"
	assertRender(t, RegexRender, input, want)

	input = "Both __this__ and __that__ will be in bold."
	want = "<p>Both <strong>this</strong> and <strong>that</strong> will be in bold.</p>"
	assertRender(t, RegexRender, input, want)

}

func Test_Regex_Header(t *testing.T) {

	input := "# This should be an h1"
	want := "<h1>This should be an h1</h1>"
	assertRender(t, RegexRender, input, want)

	input = " # This should not be a header of any kind but it is."
	want = "<h1>This should not be a header of any kind but it is.</h1>"
	assertRender(t, RegexRender, input, want)

	input = "### this should be an h3."
	want = "<h3>this should be an h3.</h3>"
	assertRender(t, RegexRender, input, want)

	input = "# this is an h1\n## this is an h2\n### this is an h3"
	want = "<h1>this is an h1</h1>\n<h2>this is an h2</h2>\n<h3>this is an h3</h3>"
	assertRender(t, RegexRender, input, want)

}

func Test_Regex_List(t *testing.T) {

	input := "* Item 1\n* Item 2"
	want := "<ul><li>Item 1</li><li>Item 2</li></ul>"
	assertRender(t, RegexRender, input, want)

}

func Test_Regex_Mixed(t *testing.T) {

	input := "# Header!\n* __Bold Item__\n* _Italic Item_"
	want := "<h1>Header!</h1><ul><li><strong>Bold Item</strong></li><li><em>Italic Item</em></li></ul>"
	assertRender(t, RegexRender, input, want)

	input = "Both __this__ and __that__ _will __be in__ bold_."
	want = "<p>Both <strong>this</strong> and <strong>that</strong> <em>will <strong>be in</strong> bold</em>.</p>"
	assertRender(t, RegexRender, input, want)
}

// Utility:

func assertRender(t testing.TB, rfunc func(string) string, input string, want string) {
	t.Helper()

	got := rfunc(input)
	if got != want {
		t.Errorf("failure:\nRender(%q)\ngot : %q\nwant: %q", input, got, want)
	}

}
