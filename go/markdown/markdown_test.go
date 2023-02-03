package markdown

import "testing"

func TestMarkdown_Original(t *testing.T) {
	for _, test := range testCases {
		if html := OriginalRender(test.input); html != test.expected {
			t.Fatalf("FAIL: Render(%q) = %q, want %q.", test.input, html, test.expected)
		}
		t.Logf("PASS: %s\n", test.description)
	}
}

func TestMarkdown_Token(t *testing.T) {
	for _, test := range testCases {
		if html := TokenRender(test.input); html != test.expected {
			t.Fatalf("FAIL: Render(%q) = %q, want %q.", test.input, html, test.expected)
		}
		t.Logf("PASS: %s\n", test.description)
	}
}

func TestMarkdown_Regex(t *testing.T) {
	for _, test := range testCases {
		if html := RegexRender(test.input); html != test.expected {
			t.Fatalf("FAIL: Render(%q) = %q, want %q.", test.input, html, test.expected)
		}
		t.Logf("PASS: %s\n", test.description)
	}
}

func BenchmarkMarkdown_Original(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping benchmark in short mode.")
	}
	// Benchmark time to parse all the test cases
	for i := 0; i < b.N; i++ {
		for _, test := range testCases {
			OriginalRender(test.input)
		}
	}
}

func BenchmarkMarkdown_Token(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping benchmark in short mode.")
	}
	// Benchmark time to parse all the test cases
	for i := 0; i < b.N; i++ {
		for _, test := range testCases {
			TokenRender(test.input)
		}
	}
}

func BenchmarkMarkdown_Regex(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping benchmark in short mode.")
	}
	// Benchmark time to parse all the test cases
	for i := 0; i < b.N; i++ {
		for _, test := range testCases {
			RegexRender(test.input)
		}
	}
}
