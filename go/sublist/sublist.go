package sublist

// Type relation is a synonym for string. No validation
// of the allowed values is done. The values are:
// equal, sublist, superlist, unequal
// type Relation string

// Sublist examines the relationship between two lists
// and returns that Relation.
func Sublist(l1, l2 []int) Relation {
	// quick guards
	if len(l1) == 0 && len(l2) == 0 {
		return RelationEqual
	}
	if len(l1) == 0 && len(l2) != 0 {
		return RelationSublist
	}
	if len(l1) != 0 && len(l2) == 0 {
		return RelationSuperlist
	}

	// simple case, lists of the same length
	// either are or aren't equal
	if slicesEqual(l1, l2) {
		return RelationEqual
	}

	// sublist & superlist checks
	if len(l1) < len(l2) {
		for i := 0; i <= len(l2)-len(l1); i++ {
			if slicesEqual(l1, l2[i:len(l1)+i]) {
				return RelationSublist
			}
		}
	} else {
		for i := 0; i <= len(l1)-len(l2); i++ {
			if slicesEqual(l2, l1[i:len(l2)+i]) {
				return RelationSuperlist
			}
		}
	}
	return RelationUnequal

}

// Function slicesEqual compares two slices.
func slicesEqual(l1 []int, l2 []int) bool {
	if len(l1) != len(l2) {
		return false
	}
	for i := 0; i < len(l1); i++ {
		if l1[i] != l2[i] {
			return false
		}
	}
	return true
}
