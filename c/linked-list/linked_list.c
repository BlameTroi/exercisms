// exercism linked list
// t.brumley, june 2022

#include "linked_list.h"
#include <stdlib.h>

typedef struct list_node {
	struct list_node *prev, *next;
	ll_data_t data;
} list_node;

#define NULL_LIST_NODE_P ((list_node *)NULL)

struct list {
	list_node *first, *last;
};

typedef struct list list;

// create new unlinked node
static list_node *newNodeWithData(ll_data_t item_data) {
	list_node *ln = malloc(sizeof(list_node));
	ln->data = item_data;
	ln->prev = NULL_LIST_NODE_P;
	ln->next = NULL_LIST_NODE_P;
	return ln;
}

// constructs a new (empty) list
struct list *list_create(void) {
	struct list *l = malloc(sizeof(struct list));
	l->first = NULL_LIST_NODE_P;
	l->last = NULL_LIST_NODE_P;
	return l;
}

// counts the items on a list
size_t list_count(const struct list *list) {
	size_t n = 0;
	list_node *ln;
	for (ln = list->first; ln != NULL_LIST_NODE_P; ln = ln->next)
		++n;
	return n;
}

// inserts item at back of a list
void list_push(struct list *list, ll_data_t item_data) {

	list_node *ln = newNodeWithData(item_data);

	// if the list is empty, just plug the shell in
	if (list->last == NULL_LIST_NODE_P) {
		list->first = ln;
		list->last = ln;
		return;
	}

	// list has at least one item, add to end and
	// update links
	list_node *last = list->last;
	ln->prev = last;
	last->next = ln;
	list->last = ln;
	return;
}

// removes item from back of a list
// bugs: behavior when the list is empty was not defined in spec
ll_data_t list_pop(struct list *list) {
	// don't crash on empty
	if (list->last == NULL_LIST_NODE_P)
		return 0;

	// data value to return
	list_node *ln = list->last;
	ll_data_t d = ln->data;

	// new end node
	list->last = ln->prev;

	// if there was no prior node, then the list held one entry but is now
	// empty, otherwise mark the new prior as last
	if (list->last == NULL_LIST_NODE_P)
		list->first = list->last;
	else
		list->last->next = NULL_LIST_NODE_P;

	// free popped node and return
	free(ln);
	return d;
}

// inserts item at front of a list
void list_unshift(struct list *list, ll_data_t item_data) {
	list_node *ln = newNodeWithData(item_data);

	// if the list is empty, just plug the shell in
	if (list->last == NULL_LIST_NODE_P) {
		list->first = ln;
		list->last = ln;
		return;
	}

	// prepend
	ln->next = list->first;
	list->first->prev = ln;
	list->first = ln;

	return;
}

// removes item from front of a list
ll_data_t list_shift(struct list *list) {
	// don't crash if empty
	if (list->first == NULL_LIST_NODE_P)
		return 0;

	// value from first node
	list_node *ln = list->first;
	ll_data_t d = ln->data;

	// update pointers for new head of list
	// single node is a special case
	if (list->first == list->last) {
		list->first = list->last = NULL_LIST_NODE_P;
	} else {
		list->first = ln->next;
		list->first->prev = NULL_LIST_NODE_P;
	}

	free(ln);
	return d;
}

// deletes a node that holds the matching data
void list_delete(struct list *list, ll_data_t data) {
	// guards

	// empty list
	if (list->first == NULL_LIST_NODE_P)
		return;

	// multiple nodes, is there a match?
	list_node *ln = list->first;
	for (; ln != NULL_LIST_NODE_P && ln->data != data; ln = ln->next)
		;

	// no match?
	if (ln == NULL_LIST_NODE_P)
		return;

	// match, update links and remove.

	// is there a prior entry?
	if (ln->prev != NULL_LIST_NODE_P) {
		ln->prev->next = ln->next;
	}

	// is there a following entry?
	if (ln->next != NULL_LIST_NODE_P) {
		ln->next->prev = ln->prev;
	}

	// adjust list head and tail if needed
	if (list->first == ln)
		list->first = ln->next;
	if (list->last == ln)
		list->last = ln->prev;

	free(ln);
	return;
}

// destroys an entire list
// list will be a dangling pointer after calling this method on it
void list_destroy(struct list *list) {
	if (list->first == NULL_LIST_NODE_P) {
		free(list);
		return;
	}
	list_node *ln, *lp;
	ln = list->first;
	while (ln != NULL_LIST_NODE_P) {
		lp = ln->next;
		free(ln);
		ln = lp;
	}
	free(list);
	return;
}
