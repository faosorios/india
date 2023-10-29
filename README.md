# Tools for influence diagnostics in statistical models

Set of routines for influence diagnostics by using case-deletion in ordinary least squares, ridge estimation and LAD regression.

## Features

Initial release (Oct 29, 2023) of [india](https://github.com/faosorios/india) package have implemented the following influence measures for objects of class lad, ols and ridge, provided by L1pack and fastmatrix, respectively.
* Cook's distances.
* Leverages (and hatvalues).
* Likelihood displacement.
* Relative change in the condition number.

Our plan in the near future is the implementation of functions to handle:
* More general models commonly used in Statistics.
* Procedures for assessing the local influence considering several perturbation schemes.

## Reference Manual

* [india.pdf](https://github.com/faosorios/india/blob/main/man/india_0.1.pdf)

## Resources

Binaries and sources for [india](https://github.com/faosorios/india) are available here (these are local files):

* [india_0.1.tar.gz](https://github.com/faosorios/india/blob/main/src/india_0.1.tar.gz) - Package sources
* [india_0.1.zip](https://github.com/faosorios/india/blob/main/binaries/india_0.1.zip) - Windows binaries

## Installation instructions

To install [india](https://github.com/faosorios/india) from its [GitHub repository](https://github.com/faosorios/india). First install the [devtools](https://devtools.r-lib.org/) package.
```r
install.packages("devtools")
```

Then install [india](https://github.com/faosorios/india) using the `install_github` function in [devtools](https://devtools.r-lib.org/)
```r
library(devtools)
install_github("faosorios/india", subdir = "pkg")
```

Alternatively, you can download the source as a tarball (.tar.gz file). Unpack this file (thereby creating a directory named, [india](https://github.com/faosorios/india)) and install the package source by executing (at the console prompt)
```
R CMD INSTALL india
```

Next, you can load the package by using the command: `library(india)`

## Providing Feedback

Please report any bugs/suggestions/improvements to [Felipe Osorio](http://fosorios.mat.utfsm.cl/). If you find these routines useful or not then please let me know. Also, acknowledgement of the use of the routines is appreciated.

### To cite the fastmatrix package in publications use:

Osorio, F. (2023). india: Tools for influence diagnostics in statistical models. 
R package version 0.1. URL: [https://github.com/faosorios/india](https://github.com/faosorios/india)

## About the Author

Felipe Osorio is an Assistant Professor at [Department of Mathematics](http://www.mat.utfsm.cl/), [Universidad Tecnica Federico Santa Maria](http://www.usm.cl/), Chile.
* Webpage: [fosorios.mat.utfsm.cl](http://fosorios.mat.utfsm.cl/)
