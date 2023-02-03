// exercism matching-brackets, are brackets matched and
// nested correctly?
// t.brumley, june 2022

#include "matching_brackets.h"

// does this open a bracket?
static bool is_opener(const char c) {
	return c == '{' || c == '(' || c == '[';
}

// does this close a bracket?
static bool is_closer(const char c) {
	return c == '}' || c == ')' || c == ']';
}

// what is the opener for this closer?
static char opener_for(const char c) {
	if (c == '}')
		return '{';
	if (c == ')')
		return '(';
	if (c == ']')
		return '[';
	return '\0';	
}

// are all the bracket characters in the string closed and nested
// properly?
bool is_paired(const char *input) {

	// simple local stack to keep track of current active brackets
	// size is arbitrary and almost certainly too big
#define MAXSTACK 100
	int sp = 0;                     // stack pointer
	char stack[MAXSTACK];           // and the stack
	char v;                         // work character
	char *curr = (char *)input;     // malleable pointer

	// scan the string and keep track of brackets
	while(*curr != '\0' && sp > -1 && sp < MAXSTACK) {
		if (is_opener(*curr))
			stack[sp++] = *curr;
		else if (is_closer(*curr)) {
			if (sp == 0)
				break;
			v = stack[--sp];
			if (v != opener_for(*curr))
				break;
		}
		curr++;
	}

	// success is being at the end of the string with an empty stack
	return (*curr == '\0' && sp == 0);
}
