package main

import (
	"fmt"
	"ledger"
	"os"
)

const (
	DateFormat = "2015-01-01"
	USD        = "USD"
	EUR        = "EUR"
	enUS       = "en-US"
	nlNL       = "nl-NL"
)

func main() {
	args := os.Args[1:]

	if len(args) == 0 {
		tryOne()
		tryTwo()
		return
	}

	fmt.Println("start...")
	for _, arg := range args {
		switch arg {
		case "zero", "0":
			fmt.Println(arg)
			tryZero()
		case "one", "1":
			fmt.Println(arg)
			tryOne()
		case "two", "2":
			fmt.Println(arg)
			tryTwo()
		case "three", "3":
			fmt.Println(arg)
			tryThree()
		}
	}
	fmt.Println("done...")
}

func tryZero() {
	entries := make([]ledger.Entry, 0)
	lines, err := ledger.FormatLedger(USD, enUS, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)
}

func tryOne() {
	entries := []ledger.Entry{
		{Date: "2015-01-01", Description: "Buy present", Change: -1000}}
	lines, err := ledger.FormatLedger(USD, enUS, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)
}

func tryTwo() {
	entries := []ledger.Entry{
		{Date: "2015-01-01", Description: "Buy present", Change: -1000}}
	lines, err := ledger.FormatLedger(EUR, nlNL, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)
}

func tryThree() {

	fmt.Println("\neur nl")
	entries := []ledger.Entry{
		{Date: "2015-01-01", Description: "eur nl negative", Change: -1000},
		{Date: "2015-01-01", Description: "eur nl negative bigger", Change: -1000000},
		{Date: "2015-01-01", Description: "eur nl positive", Change: 1000},
		{Date: "2015-01-01", Description: "eur nl positive bigger", Change: 1000000},
		{Date: "2501-01-01", Description: "eur nl zero", Change: 0},
	}
	lines, err := ledger.FormatLedger(EUR, nlNL, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)

	fmt.Println("\nusd nl")
	entries = []ledger.Entry{
		{Date: "2015-01-01", Description: "usd nl negative", Change: -1000},
		{Date: "2015-01-01", Description: "usd nl negative bigger", Change: -1000000},
		{Date: "2015-01-01", Description: "usd nl positive", Change: 1000},
		{Date: "2015-01-01", Description: "usd nl positive bigger", Change: 1000000},
		{Date: "2501-01-01", Description: "usd nl zero", Change: 0},
	}
	lines, err = ledger.FormatLedger(USD, nlNL, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)

	fmt.Println("\neur us")
	entries = []ledger.Entry{
		{Date: "2015-01-01", Description: "eur us negative", Change: -1000},
		{Date: "2015-01-01", Description: "eur us negative bigger", Change: -1000000},
		{Date: "2015-01-01", Description: "eur us positive", Change: 1000},
		{Date: "2015-01-01", Description: "eur us positive bigger", Change: 1000000},
		{Date: "2501-01-01", Description: "eur us zero", Change: 0},
	}
	lines, err = ledger.FormatLedger(EUR, enUS, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)

	fmt.Println("\nusd us")
	entries = []ledger.Entry{
		{Date: "2015-01-01", Description: "usd us negative", Change: -1000},
		{Date: "2015-01-01", Description: "usd us negative bigger", Change: -1000000},
		{Date: "2015-01-01", Description: "usd us positive", Change: 1000},
		{Date: "2015-01-01", Description: "usd us positive bigger", Change: 1000000},
		{Date: "2501-01-01", Description: "usd us zero", Change: 0},
	}
	lines, err = ledger.FormatLedger(USD, enUS, entries)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(lines)
}
