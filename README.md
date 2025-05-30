# india: Tools for influence diagnostics in statistical models

[![CRAN status](http://www.r-pkg.org/badges/version/india)](https://cran.r-project.org/package=india)
![CRAN/METACRAN](https://img.shields.io/cran/l/fastmatrix?color=informational)
[![fastmatrix](https://img.shields.io/badge/Depends-fastmatrix-orange)](https://cran.r-project.org/package=fastmatrix)
[![L1pack](https://img.shields.io/badge/Depends-L1pack-orange)](https://cran.r-project.org/package=L1pack)
![GitHub last commit](https://img.shields.io/github/last-commit/faosorios/india)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/india)](https://cran.r-project.org/package=india)

Set of routines for influence diagnostics by using case-deletion in **ordinary least squares**, **nonlinear regression**, **ridge estimation** and **LAD regression**.

## Features

Lastest release (May 03, 2025) of [india](https://github.com/faosorios/india) package have implemented the following influence measures for objects of class **nls**, **lad**, **ols** and **ridge**, provided by [L1pack](https://cran.r-project.org/package=L1pack) and [fastmatrix](https://faosorios.github.io/fastmatrix/), respectively.
* Cook's distances.
* Leverages (and hatvalues).
* Likelihood displacement.
* Relative change in the condition number.

Our plan in the near future is the implementation of functions to handle:
* General models commonly used in Statistics.
* Procedures for assessing the local influence considering several perturbation schemes.

## Reference Manual

* [india.pdf](https://cran.r-project.org/web/packages/india/india.pdf)

## Resources

Binaries and sources for [india](https://github.com/faosorios/india) are available here (these are local files):

* [india_0.1-1.tar.gz](https://github.com/faosorios/india/blob/main/src/india_0.1-1.tar.gz) - Package sources
* [india_0.1-1.zip](https://github.com/faosorios/india/blob/main/binaries/india_0.1-1.zip) - Windows binaries

Version 0.1-1 of [india](https://github.com/faosorios/india) can be found at the [CRAN package repository](https://cran.r-project.org/package=india):

* [india_0.1-1.tar.gz](https://cran.r-project.org/src/contrib/india_0.1-1.tar.gz) - Package sources
* [india_0.1-1.zip](https://cran.r-project.org/bin/windows/contrib/4.5/india_0.1-1.zip) - Windows binaries (R-release)
* [india_0.1-1.tgz](https://cran.r-project.org/bin/macosx/big-sur-arm64/contrib/4.5/india_0.1-1.tgz) - MacOS binaries (R-release, arm64)
* [india_0.1-1.tgz](https://cran.r-project.org/bin/macosx/big-sur-x86_64/contrib/4.5/india_0.1-1.tgz) - MacOS binaries (R-release, x86_64)

## Installation instructions

To install [india](https://github.com/faosorios/india) **(version 0.1-1)** from CRAN, start R and enter:
```r
install.packages("india")
```

Or install it from its [GitHub repository](https://github.com/faosorios/india). First install the [devtools](https://devtools.r-lib.org/) package.
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

Please report any bugs/suggestions/improvements to [Felipe Osorio](https://faosorios.github.io/). If you find these routines useful or not then please let me know. Also, acknowledgement of the use of the routines is appreciated.

### To cite the fastmatrix package in publications use:

Osorio, F. (2025). india: Influence diagnostics in statistical models. 
R package version 0.1-1. URL: [https://cran.r-project.org/package=india](https://cran.r-project.org/package=india)

## Papers using india
- Ogueda, A., Osorio, F. (2025). Influence diagnostics for ridge regression using the Kullback-Leibler divergence. [Statistical Papers](https://doi.org/10.1007/s00362-025-01701-1) 66, 85.

## About the Author

Felipe Osorio is an applied statistician and creator of several R packages
* Webpage: [faosorios.github.io](https://faosorios.github.io/)
