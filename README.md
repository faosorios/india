# Tools for influence diagnostics in statistical models

Set of routines for influence diagnostics by using case-deletion in ordinary least squares, ridge estimation and LAD regression.

## Features

Initial release (Oct 29, 2023) of [india](https://github.com/faosorios/india) package have implemented the following influence measures for objects of class lad, ols and ridge, provided by L1pack and fastmatrix, respectively.
* Cook's distances.
* Leverages (and hatvalues).
* Likelihood displacement.
* Relative change in the condition number.
* 
Our plan in the near future is the implementation of functions to handle:
* More general models commonly used in Statistics.
* Procedures for assessing the local influence considering several perturbation schemes.
