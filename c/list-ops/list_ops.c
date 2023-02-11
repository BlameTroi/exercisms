#include "list_ops.h"

// Function prototypes for internal functions.

// Allocate space for a list of LENGTH elements.
/* static */
/* list_t *empty_list(size_t length); */


// Create an empty list to hold the desired number of elements.
static
list_t *
empty_list(size_t length) {
  return calloc(1, sizeof(size_t) + (length * sizeof(list_element_t)));
}


// Create and populate a new list with the provided ELEMENTS.
list_t *
new_list(size_t length, list_element_t elements[]) {
  list_t *l = empty_list(length);
  for (size_t i = 0; i < length; i++) {
    l->elements[i] = elements[i];
  }
  l->length = length;
  return l;
}


// Return a new list containing the elemnts of LIST1
// followed by the elements of LIST2.
list_t *
append_list(list_t *list1, list_t *list2) {
  list_t *l = empty_list(list1->length + list2->length);
  l->length = list1->length + list2->length;
  for (size_t i = 0; i < list1->length; i++) {
    l->elements[i] = list1->elements[i];
  }
  for (size_t i = 0; i < list2->length; i++) {
    l->elements[list1->length + i] = list2->elements[i];
  }
  return l;
}


// filter list returning only values that satisfy the filter function
list_t *
filter_list(list_t *list, bool (*filter)(list_element_t)) {
  list_t *filtered = empty_list(list->length);
  // calloc(1, sizeof(size_t) + (list->length * sizeof(list_element_t)));
  if (list->length == 0) {
    return filtered;
  }
  size_t filteredCount = 0;
  for (size_t i = 0; i < list->length; i++) {
    if ((*filter)(list->elements[i])) {
      filtered->elements[filteredCount] = list->elements[i];
      filteredCount++;
    }
  }
  filtered->length = filteredCount;
  return filtered;
}


// returns the length of the list
size_t
length_list(list_t *list) {
  return list->length;
}


// return a list of elements whose values equal those of LIST after
// the MAP.
list_t *
map_list(list_t *list, list_element_t (*map)(list_element_t)) {
  list_t *mapped = empty_list(list->length);
  if (list->length == 0) {
    return mapped;
  }
  for (size_t i = 0; i < list->length; i++) {
    mapped->elements[i] = (*map)(list->elements[i]);
  }
  mapped->length = list->length;
  return mapped;
}


// folds (reduces) the LIST from the left with a function.
list_element_t
foldl_list(list_t *list, list_element_t initial,
           list_element_t (*foldl)(list_element_t,
                                   list_element_t)) {
  list_element_t folded = initial;
  if (list->length == 0) {
    return folded;
  }
  for (size_t i = 0; i < list->length; i++) {
    folded = (*foldl)(list->elements[i], folded);
  }
  return folded;
}


// folds (reduces) the LIST from the right with a function
list_element_t
foldr_list(list_t *list, list_element_t initial,
                          list_element_t (*foldr)(list_element_t,
                                                  list_element_t)) {
  list_element_t folded = initial;
  if (list->length == 0) {
    return folded;
  }
  size_t fromRight = list->length - 1;
  for (size_t i = 0; i < list->length; i++) {
    folded = (*foldr)(list->elements[fromRight-i], folded);
  }
  return folded;
}


// reverse the order of the elements of the LIST.
list_t *reverse_list(list_t *list) {
  list_t *reversed = new_list(list->length, list->elements);
  size_t revend = reversed->length - 1;
  for (size_t i = 0; i < list->length / 2; i++) {
    list_element_t swap = reversed->elements[i];
    reversed->elements[i] = reversed->elements[revend-i];
    reversed->elements[revend-i] = swap;
  }
  return reversed;
}


// destroy the entire list
// list will be a dangling pointer after calling this method on it
void delete_list(list_t *list) {
  free(list);
}
