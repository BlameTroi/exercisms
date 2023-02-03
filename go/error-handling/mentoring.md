# Mentoring

andrerfcsantos, 2d ago

Well done passing the tests!

I like the first part where you deal with the ```TransientError```.

The part where you deal with ```FrobError``` can be simplified. First note that in all the branches of the code, you are calling ```mr.Close()```. This means that no matter what happens, we want to close the resource. This can be easily done by using ```defer res.Close()``` before the deferred function on line 33. With this, you don't need to calls to ```res.Close()``` in the deferred function anymore.

Line 42 is not needed if you write line 41 as:

```go
if fe, ok = pan.(FrobError); ok {
  // ...
}
```

When using recover() it's also usual to handle the case where recover != nil in a single if, like:

```go
if pan := recover(); pan != nil {
  // ...
}
```
