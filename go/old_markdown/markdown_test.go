package markdown

import "testing"

func TestMarkdown(t *testing.T) {
	for _, test := range testCases {
		if html := Render(test.input); html != test.expected {
			t.Fatalf("FAIL: Render(%q) = %q, want %q.", test.input, html, test.expected)
		}
		t.Logf("PASS: %s\n", test.description)
	}
}

func BenchmarkMarkdown_old(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping benchmark in short mode.")
	}
	// Benchmark time to parse all the test cases
	for i := 0; i < b.N; i++ {
		for _, test := range testCases {
			OldRender(test.input)
		}
	}
}

func BenchmarkMarkdown_new(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping benchmark in short mode.")
	}
	// Benchmark time to parse all the test cases
	for i := 0; i < b.N; i++ {
		for _, test := range testCases {
			NewRender(test.input)
		}
	}
}
