package react

// Type computeCell1 is an implementation of the ComputeCell
// interface for a cell that depends on one other Cell.
type computeCell1 struct {
	oc           Cell
	fn           func(int) int
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
func (c *computeCell1) Value() int {
	nv := c.fn(c.oc.Value())
	if nv == c.value {
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
func (c *computeCell1) AddCallback(fn func(int)) Canceler {
	return addCallback(c, fn)
}
