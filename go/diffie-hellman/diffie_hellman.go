package diffiehellman

import (
	"crypto/rand"
	"math/big"
)

var one = big.NewInt(1)
var zero = big.NewInt(0)

func PrivateKey(p *big.Int) *big.Int {
	i := zero
	var err error
	for i.Cmp(one) <= 0 || i.Cmp(p) >= 0 {
		i, err = rand.Int(rand.Reader, p)
		if err != nil {
			panic(err)
		}
	}
	return i
}

func PublicKey(private, p *big.Int, g int64) *big.Int {
	i := big.NewInt(0)
	return i.Exp(big.NewInt(g), private, p)
}

func NewPair(p *big.Int, g int64) (private, public *big.Int) {
	private = PrivateKey(p)
	public = PublicKey(private, p, g)
	return private, public
}

func SecretKey(private1, public2, p *big.Int) *big.Int {
	i := big.NewInt(0)
	return i.Exp(public2, private1, p)
}
