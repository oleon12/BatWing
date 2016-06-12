# BatWing
---
```
**Package write in the R programming language**
```
```
Version 1.0.0. Beta (2016-06-10)
```

This package calculates the Lifting Surface Area (LSA) of a Bat's wing using different formulas, and also calculate the dynamics measures of the Bat's wing.

The input files for both LSA function and Dynamics function are very specific, the colums order must be the same as the example data ( https://github.com/oleon12/BatWing/tree/master/data ).

The LSA formulas which this package use are the proposal by:

- **Pirlot 1977**
  + LSA = 0.735 *B *V
- **Smith & Starrett 1979**
  + $LSA = 2 [(FA*V)+\1/2]$
