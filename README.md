# R-FieldSpectra
R library for importing and processing field spectroscopy data collected with ASD, Spectra Vista, and Spectral Evolution instruments

**Corresponding author**

Dr. Shawn Serbin

Environmental and Climate Sciences Department

Brookhaven National Laboratory

sserbin@bnl.gov

Created by: Shawn P. Serbin <sserbin@bnl.gov>


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
# Leaf level responses to light and temperature
file <- system.file("examples/Example_Leaf_Photosynthesis.R",package="CanopyPhotosynthesis")
source(file)
# This should generate two plots, one showing the An vs PAR and the second showing the temperature sensitivities of photosynthesis and key parameters such as Vcmax, Jmax, and Rd

# MORE IN DEVELOPMENT
```
