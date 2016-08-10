# MetaAnalyser

A web application to visualise meta-analysis as physical weights on scales, and its R interface.

## Installation

```r
install.packages("devtools") # if devtools not already installed
devtools::install_github('rstudio/DT')
devtools::install_github('chjackson/MetaAnalyser')

# Test with
library(MetaAnalyser)
MetaAnalyser()
```

MetaAnalyser is not yet on CRAN, as it depends on DT 0.1.40 or later.

*(Aug 2016) DT 0.2 has now been released on CRAN, so MetaAnalyser will also be sent to CRAN when I get a bit of time*
