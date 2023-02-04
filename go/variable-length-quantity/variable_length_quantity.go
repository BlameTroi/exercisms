// Package variablelengthquantiy demonstrates encoding
// and decoding to compress uint32 values into a slice
// of bytes, and back again.
package variablelengthquantity

import "errors"

// EncodeVarint encodes a uint32 into a variable length
// quantity.
func EncodeVarint(ip []uint32) []byte {
	op := make([]byte, 0, 8)
	for _, ui := range ip {
		op = append(op, encodeVarint(ui)...)
	}
	return op
}

// encodeVariant encodes a single uint32 into
// a vlq.
func encodeVarint(ip uint32) []byte {
	if ip < 128 {
		return []byte{byte(ip)}
	}
	rop := make([]byte, 0, 8)
	for ip != 0 {
		n := ip & 127
		ip = ip >> 7
		if len(rop) > 0 {
			n += 128
		}
		rop = append(rop, byte(n))
	}
	for i, j := 0, len(rop)-1; i < j; i, j = i+1, j-1 {
		rop[i], rop[j] = rop[j], rop[i]
	}
	return rop
}

// DecodeVarint decodes a variable length quantity into
// an uint32.
// bugs: does not check for overflow of output uint32
func DecodeVarint(ip []byte) ([]uint32, error) {
	op := make([]uint32, 0, 2)
	var ui uint32
	var i int
	for i = 0; i < len(ip); i++ {
		ui = ui << 7
		ui = ui + uint32(ip[i]&127)
		if ip[i]&128 == 0 {
			op = append(op, ui)
			ui = 0
		}
	}
	if ip[i-1]&128 != 0 {
		return nil, errors.New("invalid encoding")
	}
	return op, nil
}
