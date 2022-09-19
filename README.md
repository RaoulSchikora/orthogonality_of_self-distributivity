# Orthogonality of Self-Distributivity

This is an experimental code base accompanying the master thesis with the title 'On orthogonality of Self-Distributivity' by R. Schikora in Haskell.

The module Reader.hs handels input. With the parse-function a string can be converted into Term datatyp, e.g.:

```
t = parse "1.2.3.4"
```

results in 

```
t = T (T (T (V '1') (V '2')) (V '3')) (V '4')
```
