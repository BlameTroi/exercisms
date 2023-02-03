// Package circular provides a wrap around byte buffer.
package circular

import "errors"

// Buffer is a circular buffer of bytes of the specified size.
type Buffer struct {
	slots  int
	used   int
	newest int
	oldest int
	data   []byte
}

// NewBuffer returns a pointer to our circular buffer type. The
// buffer is initially empty.
func NewBuffer(size int) *Buffer {
	b := &Buffer{
		slots:  size,
		used:   0,
		newest: 0,
		oldest: 0,
		data:   make([]byte, size),
	}
	return b
}

// ReadByte returns the oldest item from the buffer, or an
// error if the buffer is empty.
func (b *Buffer) ReadByte() (byte, error) {
	if b.used == 0 {
		return 0, errors.New("buffer empty")
	}
	c := byte(0)
	if b.used == 1 {
		c = b.data[b.newest]
		b.used, b.newest, b.oldest = 0, 0, 0
		return c, nil
	}
	b.used--
	c = b.data[b.oldest]
	b.oldest++
	if b.oldest >= b.slots {
		b.oldest = 0
	}
	return c, nil
}

// WriteByte adds a new item to the buffer, or an error if
// the buffer is full.
func (b *Buffer) WriteByte(c byte) error {
	if b.used == b.slots {
		return errors.New("buffer full")
	}
	b.newest++
	if b.newest >= b.slots {
		b.newest = 0
	}
	b.data[b.newest] = c
	b.used++
	if b.used == 1 {
		b.oldest = b.newest
	}
	return nil
}

// Overwrite forces a write even when the buffer is full. The
// oldest existant byte is replaced.
func (b *Buffer) Overwrite(c byte) {
	if b.slots > b.used {
		b.WriteByte(c)
		return
	}
	b.data[b.oldest] = c
	b.newest = b.oldest
	b.oldest++
	if b.oldest >= b.slots {
		b.oldest = 0
	}
}

// Reset logically empties the buffer.
func (b *Buffer) Reset() {
	b.used = 0
	b.newest = 0
	b.oldest = 0
}

// Empty returns true if the buffer is empty.
func (b *Buffer) Empty() bool {
	return b.used == 0
}

// Full returns true if the buffer is full.
func (b *Buffer) Full() bool {
	return b.used < b.slots
}
