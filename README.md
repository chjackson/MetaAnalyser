# MetaAnalyser

A web application to visualise meta-analysis as physical weights on scales, and its R interface.

See J. Bowden and C. Jackson *Weighing evidence with the Meta-Analyser* The American Statistician (2016) Available online, [http://dx.doi.org/10.1080/00031305.2016.1165735] (http://dx.doi.org/10.1080/00031305.2016.1165735).

See also the [MetaAnalyser demo at shinyapps](https://chjackson.shinyapps.io/MetaAnalyser).

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
