// exercism word_count
// t.brumley, june 2022

// #define _DEFAULT_SOURCE
#include "word_count.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>

// utility function prototypes
static char *stringdup(const char *s);
static size_t smin(size_t a, size_t b);
static bool inTable(char *token, const word_count_word_t *table, size_t slots);

// count_words - routine to classify the unique words and their frequency in a
// sentence inputs:
//    sentence =  a null-terminated string containing that is analyzed
//
// outputs:
//    words = allocated structure to record the words found and their frequency
//    uniqueWords - number of words in the words structure
//           returns a negative number if an error.
//           words will contain the results up to that point.
int count_words(const char *sentence, word_count_word_t *words) {
	int count = 0;
	words = NULL;
	if (sentence == NULL)
		return 0;

	// result is an unordered array, we have to expand it as new
	// words are encountered.
	size_t slots = 1;
	size_t next = 0;
	words = calloc(slots, sizeof(word_count_word_t));

	// strtok updates the string, so we'll need to make a copy
	char *acopy = stringdup(sentence);
	char *inprogress = acopy;
	char *token = NULL;
	char *delims = " \t\n";
	while ((token = strtok(inprogress, delims)) != NULL) {
		inprogress = NULL; // strtok wants null on subsequent calls	
		// debug ...
		printf("%s\n", token);
		if (!inTable(token, words, slots)) {
			if (next == slots) {
				// resize array
				// reallocarray would be a better solution, but feature test
				// macro issues get in the way
				word_count_word_t *newWords;
				newWords = calloc(slots + slots, sizeof(word_count_word_t));
				memcpy(newWords, words, slots * sizeof(word_count_word_t));
				void *temp = words;
				words = newWords;
				free(temp);
				slots += slots;
				words[next].count = 1;
				memcpy(words[next].text, token, smin(MAX_WORD_LENGTH, strlen(token)));
				// strncpy((words[next]).text, token, MAX_WORD_LENGTH);
				if (strlen(token) > MAX_WORD_LENGTH) {

				}
			}
			printf("\tdup\n");
		} else {
			
		}
		
		
		words[0].count = 1;
		// more to come
	}

	// clean up and return
	free(acopy);
	return count;
}

// utility function implementations

// strdup would be preferred but that requires messing with the
// provided makefile for feature test macros.
static char *stringdup(const char *s) {
	char *t = malloc(strlen(s));
	memcpy(t, s, strlen(s));
	return t;
}

static size_t smin(size_t a, size_t b) {
	if (a <= b)
		return a;
	return b;
}

static bool inTable(char *token, const word_count_word_t *table, size_t slots) {
	if (token == NULL || table == NULL)
		return false;

	int i = slots;
	while (i > 0 && table[i].text[0] != '\0') {
		if (strcmp(table[i].text, token) == 0)
			return true;
	}
	return false;
}
