# Sea-level monitor

Tools and applications used to monitor sea-level rise in the Netherlands. New github following up the applications of https://github.com/openearth/sealevel. Sea level analysis and monitoring is set up in Rmarkdown files.

## Analysis

Sea level analysis of the six main tide stations in the Netherlands is presented in this document. In the document the following steps are taken:

- Visual inspection of water level time series
- Global Tide and Surge Model calculations of surge are subtracted from the water level measurements
- application of different regression models 
  + linear
  + broken linear (from 1993)
  + broken quadratic (from 1960)
- check preconditions for application of models
- selection of model for the Sea Level Monitor based on 
  + significance of breakpoint
  + AIC criteria
  + ANOVA test between models
  
## Monitor

Sea Level Monitor document presents the results for an average of the Dutch stations, using the preferred model (from Sea Level Analysis)

## Data

Description of data folder

## Help

initialize R env
When cloning the repo, initialize the R env by:

``` shell
renv::activate()
renv::restore()
```

## License


## Contact 

Contact maintainer for more information