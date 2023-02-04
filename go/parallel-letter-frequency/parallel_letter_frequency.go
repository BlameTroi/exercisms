// Package letter implements a rune counter on strings, and
// demonstrates running in "parallel" using goroutines.
package letter

import "sync"

// FreqMap records the frequency of each rune in a given string.
type FreqMap map[rune]int

// Frequency counts the frequency of each rune in a given string
// and returns a FreqMap.
func Frequency(s string) FreqMap {
	m := FreqMap{}
	for _, r := range s {
		m[r]++
	}
	return m
}

// ConcurrentFrequency runs Frequency on several strings at once
// using goroutines, WaitGroup, and channels.
func ConcurrentFrequency(chunks []string) FreqMap {
	// Guards and special cases:
	if len(chunks) == 0 {
		return FreqMap{}
	}
	if len(chunks) == 1 {
		return Frequency(chunks[0])
	}

	// Start multiple frequency counters.
	wg := sync.WaitGroup{}
	ch := make(chan FreqMap, len(chunks))
	for _, chunk := range chunks {
		wg.Add(1)
		go func(s string) {
			defer wg.Done()
			ch <- Frequency(s)
		}(chunk)
	}
	wg.Wait()

	// When all counters have finished, merge
	// their results and return.
	maps := []FreqMap{}
	for i := 0; i < len(chunks); i++ {
		maps = append(maps, <-ch)
	}
	return mergeFreqMaps(maps)
}

// Function mergeFreqMaps combines an aray of
// FreqMaps into one.
func mergeFreqMaps(maps []FreqMap) FreqMap {
	o := FreqMap{}
	for _, m := range maps {
		for r, c := range m {
			o[r] += c
		}
	}
	return o
}
