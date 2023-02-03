package ledger

import (
	"errors"
	"fmt"
	"sort"
	"strconv"
	"strings"
)

// well this is a big heaping mess. there are obvious signs of copy and paste,
// lack of knowledge of the libraries, and so on. the first task is going to be
// to clean up code "in the small". doing this will help me become more familiar
// with the code and likely reduce the visual clutter so the needed logic can be
// seen.
//
// it's like sculpting: cut away everything that isn't part of a good program.
//
// several "// note:" markers are left in various places as
//
// refactoring log:
// 1 - review, comment, extract header creation
// 2 - clean up header creation to use fmt.Sprintf instead of concatenation,
//     begin doing the same throughout the program
// 3 - rename some variables in the detail line processing to improve readability
//     and simplify some of the error checking.
//     identify more opportunities for sprintf, sscanf.
//     use a string builder.
//     check for valid currency and locale at the beginning.
// 4 - replace existing sort with sort.Slice(), much more readable
//     move error checking in the detail line loop up to guard clauses, i'll
//     trade the extra pass over the entries for cleaner code
//     remove the error field from the detail line chan struct
//     renamed a few more variables
//     removed recursive call to handle no detail entries more clearly
// 5 - remove use of go routine for detail lines. this increased memory usage
//     (not expected) but more than doubled throughput (a bit better than
//     expected).
//     started extracting code, modularization & stepwise refinement ftw!
// 6 - extract date validation and expand it, this ate some of the throughput
//     improvement and increased memory usage a good bit.
//     more renames
//     currency formatting--it turns out fmt.Sprintf doesn't support business
//     formatting. A usable but not complete currency formatting routine with
//     a configuration structure created. it reads better than the old code
//     but it needs more work. that's an offline project relative to this
//     exercise. tests passing so i'm calling it.

type Entry struct {
	Date        string
	Description string
	Change      int
}

var (
	ErrBadLocale   error = errors.New("locale not supported")
	ErrBadCurrency error = errors.New("currency not supported")
	ErrBadDate     error = errors.New("invalid date")
)

func FormatLedger(currency string, locale string, originalEntries []Entry) (string, error) {

	// validate input, return any error
	err := validateEntries(currency, locale, originalEntries)
	if err != nil {
		return "", err
	}

	// start with column header.
	sb := strings.Builder{}
	sb.WriteString(makeHeader(locale))

	// return lone header if there are no entries
	if len(originalEntries) == 0 {
		return sb.String(), nil
	}

	// make a working copy of the detail entries and sort them
	var entries []Entry = sortEntries(originalEntries)

	// note: removed concurrency code, it's slower and more complex
	//       and really not needed for this sort of application.

	// format each entry into a line of output
	for _, entry := range entries {

		de := fitString(entry.Description, 25)
		dt := formatDate(entry.Date, locale)

		var a string
		curfmt := curfmtDefault
		if locale == "nl-NL" && currency == "EUR" {
			curfmt = fullEuro
		}
		if locale == "en-US" && currency == "USD" {
			curfmt = fullUS
		}
		if locale == "en-US" && currency == "EUR" {
			curfmt = partUSEuro
		}
		if locale == "nl-NL" && currency == "USD" {
			curfmt = partNLUsd
		}
		a = formatCurrency(entry.Change, curfmt)

		sb.WriteString(fmt.Sprintf("%s | %-25s | %13s\n", dt, de, a))
	}

	return sb.String(), nil
}

// Truncate or pad string to fit in length. Truncation adds ellipsis to indicate the loss.
func fitString(s string, l int) string {
	if len(s) > l {
		return s[:l-3] + "..."
	} else {
		return s + strings.Repeat(" ", l-len(s))
	}
}

// Reformat date to match locale
func formatDate(di string, locale string) string {
	year, month, day := di[0:4], di[5:7], di[8:10]
	switch locale {
	case "nl-NL":
		return fmt.Sprintf("%s-%s-%s", day, month, year)
	case "en-US":
		return fmt.Sprintf("%s/%s/%s", month, day, year)
	default:
		// this is a should not occur, but coded for completeness
		return di
	}
}

// Validate input
func validateEntries(currency string, locale string, originalEntries []Entry) error {

	if locale != "nl-NL" && locale != "en-US" {
		return ErrBadLocale
	}
	if currency != "EUR" && currency != "USD" {
		return ErrBadCurrency
	}
	for _, e := range originalEntries {
		err := validateDate(e.Date)
		if err != nil {
			return err
		}
	}

	return nil
}

// is valid digits?
func allDigits(s string) bool {
	for _, c := range s {
		if c < '0' || c > '9' {
			return false
		}
	}
	return true
}

// in a real application, the data would likely have been validated
// before being sent, but since the base code did error checks,
// we'll do error checks. the base checked for separators field
// width but not much else. a more complete validation is not
// required but i did it for the exercise.
//
// regex is overkill since the date format is expected to be
// an ISO date. while creating a regex do do these checks is
// possible and involves fewer lines of code, it won't be readable
// to most people.
//
var daysInMonth []int = []int{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
var daysInMonthLeap []int = []int{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}

func validateDate(s string) error {

	if len(s) != 10 {
		return ErrBadDate
	}

	var yyyy, mm, dd, d1, d2 string
	n, err := fmt.Sscanf(s, "%4s%1s%2s%1s%2s", &yyyy, &d1, &mm, &d2, &dd)
	if n != 5 || err != nil {
		return ErrBadDate
	}

	if d1 != "-" || d2 != "-" {
		return ErrBadDate
	}

	if !allDigits(yyyy) || !allDigits(mm) || !allDigits(dd) {
		return ErrBadDate
	}

	y, _ := strconv.Atoi(yyyy)
	m, _ := strconv.Atoi(mm)
	d, _ := strconv.Atoi(dd)

	if m < 1 || m > 12 {
		return ErrBadDate
	}

	daysTable := daysInMonth
	if y%400 == 0 || (y%4 == 0 && y%100 != 0) {
		daysTable = daysInMonthLeap
	}
	if d > daysTable[m] {
		return ErrBadDate
	}

	return nil
}

// Create locale appropriate column headers
func makeHeader(locale string) string {
	var r []interface{}
	switch locale {
	case "nl-NL":
		r = []interface{}{"Datum", "Omschrijving", "Verandering"}
	case "en-US":
		r = []interface{}{"Date", "Description", "Change"}
	default:
		// note: this is a should not occur, but put here in case a new
		//       locale is added but not completely implemented
		r = []interface{}{"?", "invalid locale! " + locale, "?"}
	}
	return fmt.Sprintf("%-10s | %-25s | %s\n", r...)
}

// Make a working copy of the entries and sort them into
// ascending order
func sortEntries(originalEntries []Entry) []Entry {

	entries := append([]Entry{}, originalEntries...)

	// sort key is date+description+change
	fsortEntries := func(i, j int) bool {
		if entries[i].Date > entries[j].Date {
			return false
		}
		if entries[i].Date < entries[j].Date {
			return true
		}
		if entries[i].Description > entries[j].Description {
			return false
		}
		if entries[i].Description < entries[j].Description {
			return true
		}
		if entries[i].Change > entries[j].Change {
			return false
		}
		return entries[i].Change < entries[j].Change
	}

	sort.Slice(entries, fsortEntries)
	return entries
}

// Currency Formatting
var fullEuro *currencyFmt = &currencyFmt{
	negative:  negativeTail,
	decimal:   decimalComma,
	thousands: thousandsPoint,
	currency:  currencyEUR,
	space:     currencySpaceYes,
}
var fullUS *currencyFmt = &currencyFmt{
	negative:  negativeWrap,
	decimal:   decimalPoint,
	thousands: thousandsComma,
	currency:  currencyUSD,
	space:     currencySpaceNo,
}
var partUSEuro *currencyFmt = &currencyFmt{
	negative:  negativeWrap,
	decimal:   decimalPoint,
	thousands: thousandsComma,
	currency:  currencyEUR,
	space:     currencySpaceNo,
}
var partNLUsd *currencyFmt = &currencyFmt{
	negative:  negativeTail,
	decimal:   decimalComma,
	thousands: thousandsPoint,
	currency:  currencyUSD,
	space:     currencySpaceYes,
}
var curfmtDefault *currencyFmt = &currencyFmt{
	negative:  negativeLead,
	decimal:   decimalPoint,
	thousands: 0,
	currency:  0,
	space:     0,
}

const (
	negativeWrap = 1
	negativeLead = 2
	negativeTail = 3

	decimalPoint = 1
	decimalComma = 2

	thousandsPoint = 1
	thousandsComma = 2

	currencyEUR = 1
	currencyUSD = 2

	currencySpaceYes = 1
	currencySpaceNo  = 2

	symEUR = '€'
	symUSD = '$'
)

type currencyFmt struct {
	negative  int
	decimal   int
	thousands int
	currency  int
	space     int
}

// formatCurrency an integer (fixed point, 2 places assumed) as currency.
// The standard fmt library does not support business oriented
// formatting, so this is limited implementation.
func formatCurrency(i int, cf *currencyFmt) string {

	cur := ' '
	if cf.currency == currencyEUR {
		cur = symEUR
	}
	if cf.currency == currencyUSD {
		cur = symUSD
	}

	dec := ' '
	if cf.decimal == decimalPoint {
		dec = '.'
	}
	if cf.decimal == decimalComma {
		dec = ','
	}

	tho := ' '
	if cf.thousands == thousandsComma {
		tho = ','
	}
	if cf.thousands == thousandsPoint {
		tho = '.'
	}

	spc := cf.space == currencySpaceYes

	neg := 0
	w := i
	if w < 0 {
		w = -w
		neg = cf.negative
	}

	whole := w / 100
	cents := w % 100
	sb := strings.Builder{}
	if neg == negativeWrap {
		sb.WriteRune('(')
	}
	if neg == negativeLead {
		sb.WriteRune('-')
	}
	if cur != ' ' {
		sb.WriteRune(cur)
	}
	if spc {
		sb.WriteRune(' ')
	}

	s := fmt.Sprintf("%d%s%02d", whole, string(dec), cents)
	if whole > 999 && tho != ' ' {
		digits := make([]rune, 0, 10)
		numdigits := 0
		for whole != 0 {
			dig := whole % 10
			whole = whole / 10
			digits = append(digits, rune('0'+dig))
			numdigits++
			if numdigits%3 == 0 && whole > 0 {
				digits = append(digits, tho)
			}
		}
		for i, j := 0, len(digits)-1; i < j; i, j = i+1, j-1 {
			digits[i], digits[j] = digits[j], digits[i]
		}
		s = fmt.Sprintf("%s%s%02d", string(digits), string(dec), cents)
	}

	sb.WriteString(s)

	if neg == negativeTail {
		sb.WriteRune('-')
	}
	if neg == negativeWrap {
		sb.WriteRune(')')
	}
	if cf.negative == negativeWrap && i >= 0 {
		sb.WriteRune(' ')
	}
	if cf.negative == negativeTail && i >= 0 {
		sb.WriteRune(' ')
	}
	return sb.String()
}
