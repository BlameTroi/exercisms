package react

// Type inputCell and its methods implement the Cell and InputCell
// interfaces.
type inputCell struct {
	value        int
	dependencies []Cell
}

// Value returns the current value of the Cell.
func (c *inputCell) Value() int {
	return c.value
}

// SetValue assigns a new value and notifies any registered
// dependents if the new value differs from the old value.
func (c *inputCell) SetValue(v int) {
	if c.value == v {
		return
	}
	c.value = v
	notifyDependents(c.dependencies)
}
