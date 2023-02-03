package react

// Type computeCell2 is an implementation of the ComputeCell
// interface for a cell that depends on two other Cells.
type computeCell2 struct {
	oc1, oc2     Cell
	fn           func(int, int) int
	value        int
	callbacks    int
	callback     map[int]func(int)
	dependencies []Cell
}

// Value returns the current value of the ComputeCell and if
// the current value differs from the prior value, will notify
// any dependents of the change and then process any callbacks.
//
// SIDE EFFECTS!
func (c *computeCell2) Value() int {
	nv := c.fn(c.oc1.Value(), c.oc2.Value())
	if c.value == nv {
		return c.value
	}
	c.value = nv
	notifyDependents(c.dependencies)
	for _, fcb := range c.callback {
		fcb(c.value)
	}
	return c.value
}

// AddCallback registers a function to be called when this
// ComputeCell changes value and returns a Canceler to
// remove the callback when no longer needed.
func (c *computeCell2) AddCallback(fn func(int)) Canceler {
	return addCallback(c, fn)
}
