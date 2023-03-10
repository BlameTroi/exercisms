package brackets

// Source: exercism/problem-specifications
// Commit: 51418ec bracket-push: rename to matching-brackets (#1501)
// Problem Specifications Version: 2.0.0

type bracketTest struct {
	input    string
	expected bool
}

var testCases = []bracketTest{
	{
		// paired square brackets
		"[]",
		true,
	},
	{
		// empty string
		"",
		true,
	},
	{
		// unpaired brackets
		"[[",
		false,
	},
	{
		// wrong ordered brackets
		"}{",
		false,
	},
	{
		// wrong closing bracket
		"{]",
		false,
	},
	{
		// paired with whitespace
		"{ }",
		true,
	},
	{
		// partially paired brackets
		"{[])",
		false,
	},
	{
		// simple nested brackets
		"{[]}",
		true,
	},
	{
		// several paired brackets
		"{}[]",
		true,
	},
	{
		// paired and nested brackets
		"([{}({}[])])",
		true,
	},
	{
		// unopened closing brackets
		"{[)][]}",
		false,
	},
	{
		// unpaired and nested brackets
		"([{])",
		false,
	},
	{
		// paired and wrong nested brackets
		"[({]})",
		false,
	},
	{
		// paired and incomplete brackets
		"{}[",
		false,
	},
	{
		// too many closing brackets
		"[]]",
		false,
	},
	{
		// math expression
		"(((185 + 223.85) * 15) - 543)/2",
		true,
	},
	{
		// complex latex expression
		"\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)",
		true,
	},
	{
		`(let ((value ""))
(defun property (&key is)
  (cond
	(is (setf value is))
	(t value)))
;;; here is getter and setter functions
(defun get-property ()
  value)
(defun set-property (data)
  (setf value data)))

(define-test get_set-property
(assert-equal "" (get-property))
(assert-equal "property" (set-property "property"))
(assert-equal "property" (get-property))
(assert-equal "" (set-property "")))

(define-test property
(assert-equal "" (property))
(assert-equal "property" (property :is "property"))
(assert-equal "property" (property))
(assert-equal "" (property :is "")))`,
		true,
	},
}
