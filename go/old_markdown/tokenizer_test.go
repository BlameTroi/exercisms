package markdown

import "testing"

// These tests are ones written to drive me through
// debugging during the refactoring. The combined tests in
// markkdown_test.go and cases_test.go are the acceptance
// tests for the final implementation as provided by exercism.
//
// The tests that exercism provides are not written for TDD,
// so for this problem I've decided to use individual tests
// to help work through the code.

func Test_Empty(t *testing.T) {

	assertRender(t, "", "<p></p>")

}

func Test_Paragraph(t *testing.T) {

	input := "This will become a paragraph."
	want := "<p>This will become a paragraph.</p>"
	assertRender(t, input, want)

	input = "This will become\na paragraph."
	want = "<p>This will become\na paragraph.</p>"
	assertRender(t, input, want)

}

func Test_Italics(t *testing.T) {

	input := "_This will be italic._"
	want := "<p><em>" + "This will be italic." + "</em></p>"
	assertRender(t, input, want)

}

func Test_Header(t *testing.T) {

	input := "# This should be an h1"
	want := "<h1>This should be an h1</h1>"
	assertRender(t, input, want)

	input = " # This should not be a header of any kind but it is."
	want = " <h1>This should not be a header of any kind but it is.</h1>"
	assertRender(t, input, want)

	input = "### this should be an h3."
	want = "<h3>this should be an h3.</h3>"
	assertRender(t, input, want)

}

func Test_List(t *testing.T) {

	input := "* Item 1\n* Item 2"
	want := "<ul><li>Item 1</li><li>Item 2</li></ul>"
	assertRender(t, input, want)

}

func Test_Mixed(t *testing.T) {

	input := "# Header!\n* __Bold Item__\n* _Italic Item_"
	want := "<h1>Header!</h1><ul><li><strong>Bold Item</strong></li><li><em>Italic Item</em></li></ul>"
	assertRender(t, input, want)

}

// calling original renderer for comparison purposes
func Test_HeaderOld(t *testing.T) {

	// old render preserves the leading space, but i don't think it should
	input := " # this should not be a header but it is"
	want := " <h1>this should not be a header but it is</h1>"
	got := OldRender(input)
	if got != want {
		t.Errorf("fail, %q %q %q", input, got, want)
	}

	input = "# This should be an h1."
	want = "<h1>This should be an h1.</h1>"
	got = OldRender(input)
	if got != want {
		t.Errorf("fail, %q %q %q", input, got, want)
	}

	input = "### this should be an h3."
	want = "<h3>this should be an h3.</h3>"
	got = OldRender(input)
	if got != want {
		t.Errorf("fail, %q %q %q", input, got, want)
	}

}

// Utility:

func assertRender(t testing.TB, input string, want string) {
	t.Helper()

	got := Render(input)
	if got != want {
		t.Errorf("failure:\nRender(%q)\ngot : %q\nwant: %q", input, got, want)
	}

}
