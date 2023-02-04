package react

// Type reactor is an implementation of the Reactor interface.
// As I couldn't think of a good reason to put any state into
// the implimentation, it is merely an anchor for factory
// methods.
type reactor struct {
}

// New returns a reactor instance.
func New() Reactor {
	r := &reactor{}
	return r
}

// CreateInput returns a new InputCell holding an initial value.
func (r *reactor) CreateInput(v int) InputCell {
	c := &inputCell{
		value: v,
	}
	c.dependencies = make([]Cell, 0)
	return c
}

// CreateCompute1 returns a new ComputeCell depending upon one other
// Cell.
func (r *reactor) CreateCompute1(oc Cell, fn func(int) int) ComputeCell {
	c := &computeCell1{
		oc: oc,
		fn: fn,
	}
	addDependency(Cell(oc), Cell(c))
	c.dependencies = make([]Cell, 0)
	c.callback = make(map[int]func(int))
	c.Value()
	return c
}

// CreateCompute2 returns a new ComputeCell depending upon two other
// Cells.
func (r *reactor) CreateCompute2(oc1 Cell, oc2 Cell, fn func(int, int) int) ComputeCell {
	c := &computeCell2{
		oc1: oc1, oc2: oc2, fn: fn}
	c.dependencies = make([]Cell, 0)
	addDependency(Cell(oc1), Cell(c))
	addDependency(Cell(oc2), Cell(c))
	c.callback = make(map[int]func(int))
	c.Value()
	return c
}

// addDependency registers a Cell as a depdent of another.
func addDependency(dependee, dependent Cell) {
	switch c := dependee.(type) {
	case *inputCell:
		c.dependencies = append(c.dependencies, Cell(dependent))
	case *computeCell1:
		c.dependencies = append(c.dependencies, Cell(dependent))
	case *computeCell2:
		c.dependencies = append(c.dependencies, Cell(dependent))
	}
}

// notifyDepdents is called by a Cell to inform any depdent that
// an update has occurred. I had tried to use a separate update
// function for the notification, but there were timing issues
// with dependency chains. Using Value avoids this but offends
// my sensabilities.
func notifyDependents(d []Cell) {
	for _, cc := range d {
		switch x := cc.(type) {
		case *computeCell1:
			x.Value()
		case *computeCell2:
			x.Value()
		}
	}
}

// addCallback allows a function to be called when the value of
// a ComputeCell changes. A Canceler function is returned to
// remove the callback when it is no longer needed.
func addCallback(c ComputeCell, fn func(int)) Canceler {
	var i int
	var callback map[int]func(int)

	switch c := c.(type) {
	case *computeCell1:
		c.callbacks++
		i = c.callbacks
		callback = c.callback
	case *computeCell2:
		c.callbacks++
		i = c.callbacks
		callback = c.callback
	}

	callback[i] = fn
	// closure behavior allows the following
	return &canceler{func() { delete(callback, i) }}
}

// Type canceler supports the Canceler interface.
type canceler struct {
	fn func()
}

// Cancel is returned from an AddCallback to allow
// the callback to be removed.
func (c *canceler) Cancel() { c.fn() }
