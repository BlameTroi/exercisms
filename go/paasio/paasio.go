// Package paasio provides support for io based billing of a cloud service.
package paasio

import (
	"io"
	"sync"
)

// The counter type implements ReadWriteCounter.
type counter struct {
	mu     sync.Mutex
	read   func(p []byte) (int, error)
	write  func(p []byte) (int, error)
	rbytes int64
	rops   int
	wbytes int64
	wops   int
}

// Read will read from an io.Reader and accumulate
// operation and byte counts for billing.
func (c *counter) Read(p []byte) (len int, err error) {
	len, err = c.read(p)
	if err != nil {
		return 0, err
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.rops += 1
	c.rbytes += int64(len)
	return len, nil
}

// ReadCount returns read statistics for billing.
func (c *counter) ReadCount() (int64, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.rbytes, c.rops
}

// Write will write to an io.Writer and accumulate
// operation and byte counts for billing.
func (c *counter) Write(p []byte) (len int, err error) {
	len, err = c.write(p)
	if err != nil {
		return 0, err
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.wops += 1
	c.wbytes += int64(len)
	return len, nil
}

// WriteCount returns write statistics for billing.
func (c *counter) WriteCount() (int64, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.wbytes, c.wops
}

// NewWriteCounter returns an instance of our billing
// counter primed for writes.
func NewWriteCounter(w io.Writer) WriteCounter {
	c := &counter{write: w.Write}
	return c
}

// NewReadCounter returns an instance of our billing
// counter primed for reads.
func NewReadCounter(r io.Reader) ReadCounter {
	c := &counter{read: r.Read}
	return c
}

// NewReadWriteCounter returns an instance of our billing
// counter primed for both reads and writes.
func NewReadWriteCounter(rw io.ReadWriter) ReadWriteCounter {
	c := &counter{read: rw.Read, write: rw.Write}
	return c
}
