# R-FieldSpectra
R library for importing and processing field spectroscopy data collected with ASD, Spectra Vista, and Spectral Evolution instruments

**Corresponding author**

Dr. Shawn Serbin <br>
Environmental and Climate Sciences Department <br>
Brookhaven National Laboratory <br>
sserbin@bnl.gov <br>
Created by: Shawn P. Serbin <sserbin@bnl.gov> <br>


## Installation
Easiest way to install is via `install_github` from the `devtools` package.

```R
install.packages('devtools') # if you haven't already installed this package and dependencies
library(devtools) # load the library
install_github("serbinsh/R-FieldSpectra") # install the package from GitHub
```

If you want a specific branch, do `install_github(..., ref="<branch>")`.

From there, you should be able to load the package in your typical R session.

## Quick example runs (after installation)
```R
### Extract metadata
# ASD
file <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpectra")
output <- extract.metadata(file,instrument="ASD")

# Spectral Evolution
file <- system.file("extdata/cvars_grape_leaf1_lc_rg_01236.sed",package="FieldSpectra")
output <- extract.metadata(file,instrument="SE")

# MORE IN DEVELOPMENT
```

### Source Code Citation
[![DOI](https://zenodo.org/badge/9887372.svg)](https://zenodo.org/badge/latestdoi/9887372)

