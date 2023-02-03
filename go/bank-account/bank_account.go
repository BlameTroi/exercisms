package account

import "sync"

// API:
//
// Open(initialDeposit int64) *Account
// (*Account) Close() (payout int64, ok bool)
// (*Account) Balance() (balance int64, ok bool)
// (*Account) Deposit(amount int64) (newBalance int64, ok bool)
//
// If Open is given a negative initial deposit, it must return nil.
// Deposit must handle a negative amount as a withdrawal. Withdrawals must
// not succeed if they result in a negative balance.
// If any Account method is called on an closed account, it must not modify
// the account and must return ok = false.

// The tests will execute some operations concurrently. You should strive
// to ensure that operations on the Account leave it in a consistent state.
// For example: multiple goroutines may be depositing and withdrawing money
// simultaneously, two withdrawals occurring concurrently should not be able
// to bring the balance into the negative.

// If you are new to concurrent operations in Go it will be worth looking
// at the sync package, specifically Mutexes:
//
// https://golang.org/pkg/sync/
// https://tour.golang.org/concurrency/9
// https://gobyexample.com/mutexes

type Account struct {
	open    bool
	balance int64
	mu      sync.Mutex
}

func Open(deposit int64) *Account {
	if deposit < 0 {
		return nil
	}
	return &Account{
		open:    true,
		balance: deposit,
		mu:      sync.Mutex{},
	}
}

func (a *Account) Close() (payout int64, ok bool) {
	a.mu.Lock()
	defer a.mu.Unlock()
	if !a.open {
		return 0, false
	}
	a.open = false
	return a.balance, true
}

func (a *Account) Balance() (balance int64, ok bool) {
	a.mu.Lock()
	defer a.mu.Unlock()
	if !a.open {
		return 0, false
	}
	return a.balance, true
}

func (a *Account) Deposit(amount int64) (newBalance int64, ok bool) {
	a.mu.Lock()
	defer a.mu.Unlock()
	if !a.open {
		return 0, false
	}
	if a.balance+amount < 0 {
		return a.balance, false
	}
	a.balance += amount
	return a.balance, true
}
