// Package erratum demonstrates recovery from a panic
// and other error handling techniques.
package erratum

// Use attempts to open, frob, and close a resource
// and insure that any errors on the operations are
// handled appropriately, leaving the resource in a
// consistent closed state.
//
// NOTE: err is expliciitly named in the return
// value signature so that deferred functions can
// modify it if needed.
func Use(fn ResourceOpener, s string) (err error) {

	var mr Resource

	// Open the resource allowing for non-fatal errors.
	// on TransientError retry until we are successful
	// or a different error occurs.
	mr, err = fn()
	for err != nil {
		_, ok := err.(TransientError)
		if !ok {
			return err
		}
		mr, err = fn()
	}

	// Make sure we close.
	defer mr.Close()

	// Defered error handler. Will rollback an error
	// on Frob prior to closing if Frob had a problem.
	// Update our outer function's err if appropriate.
	defer func() {
		var pan interface{}
		var ok bool
		var fe FrobError
		if pan = recover(); pan != nil {
			if fe, ok = pan.(FrobError); ok {
				mr.Defrob(fe.defrobTag)
				err = fe.inner
			} else {
				err, _ = pan.(error)
			}
		}
	}()

	mr.Frob(s)

	return err
}
