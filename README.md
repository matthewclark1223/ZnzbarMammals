# Combining widescale interview data with satellite imagery to obtain low-cost information on mammal distribution and threats'

The code provided here allows interested readers to reproduce the primary analyses used in this manuscript. Ancillary code and data (e.g., cleaning the Google Buildings data) are ommitted and can be ontained upon request.


There are three R scripts and two datasets included in this repository. Users may choose to use these scripts or to analyze and visualize the data in other ways that suit their needs.

### Files:

* BivariateMap.R - Code for making the bivariate map
* MapsCode.R - Code for making single variable maps
* mammalsFinalAnalysis.R - Code for running the statistical models and plotting results. Note that users must have STAN installed on their computer. Instruction sfor installing stan can be found here: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
* Present.Species.median.csv - Data on species observations and covariates
* Hunted.Species.median.csv - Data on species hunting and covariates

Also note that all scripts require users to have the 'tidyverse' suite of packages installed. 

**Matt Clark, Archie Wills-Johnson 18 June 2024**
