# Orthogonality of Self-Distributivity

This is an experimental code base accompanying the master thesis with the title 'On orthogonality of Self-Distributivity' by R. Schikora in Haskell.

## Types

### Term

A term is defined by the following data-typ:

```
data Term = 
    V String
    | T Term Term
```

### Position

Positions are represented as lists containing 0 and 1. The user has to ensure that no other `Int` 
is contained in that list, as behaviour is undefined for those cases. 

### Trek

A Trek is defined as a tuple of a Term and a list of position tuples. With the funciton `defCheck` we can check, if a trek obeys SD1, SD2, SD3 and SD4,
i.e., if the tuple of Term and position tuples indeed defines a trek according to the definition by R. Schikora.

## Modules

### Reader

The module Reader.hs handels input. With the parse-function a string can be converted into Term datatyp, e.g.:

```
t = parse "1.2.3.4"
```

results in 

```
t = T (T (T (V '1') (V '2')) (V '3')) (V '4')
```

and represents the term `1*2*3*4`.

### Rd

The module Rd.hs provides basic right self-distributive (RD) operations. The most basic RD-operation is

```
rd t
```

Applying the function `rd` to the term `t` from above results in the term `1*2*4*(3*4)`. With `rdat` we specify at which position we want to apply the RD-operation, e.g.:

```
rdat t [0]
```

results in `1*3*(2*3)*4`. With `trace` we can trace a trek by a step defined as a position, e.g.:

```
trace (t,[([],[0]),([],[0,0]),([0],[0,0])]) [0]
```

results in the trek `(1*3*(2*3)*4,[([],[0,0]),([],[0,1]),([],[0])])` for the term `t` from above. With the function `develop` we can develop a trek according to a given strategy. The strategy is defined as an Int:
- 0 default strategy: the list of position tuples is developped from left to right.
- 1 in-order strategy: the list is developped according to the least in-order step.
- 2 inner most strategy: the list is developped according to an inner most step.
- 3 reversed in-order strategy: the list is developped according to the greatest in-order step.
- 4 outer most strategy: the list is developped according to an outer most step.

```
develop (t,[([],[0]),([],[0,0]),([0],[0,0])]) 3
```

results in a complete development of the given trek according to the reversed in-order strategy, which is presented by a list of tuples consisting of a trek and the position where the RD step was applied. In the case above the result is `[((1*2*3*4,[([],[0]),([],[0,0]),([0],[0,0])]),[]),((1*2*4*(3*4),[([0],[0,0]),([],[0,0])]),[0]),((1*4*(2*4)*(3*4),[([],[0])]),[]),((1*4*(3*4)*(2*4*(3*4)),[]),[])]`.

### InductiveDef

The module InductiveDef.hs represents first tries of finding an inductive definition of the target of a complete development of a trek. With the function `complete` the author conjectures an inductive approach. However, attempts to prove its correctness remain unsuccessful so far.
