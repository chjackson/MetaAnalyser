# MetaAnalyser

A web application to visualise meta-analysis as physical weights on scales, and its R interface.

See J. Bowden and C. Jackson *Weighing evidence with the Meta-Analyser*, The American Statistician (2016) Available online, [http://dx.doi.org/10.1080/00031305.2016.1165735] (http://dx.doi.org/10.1080/00031305.2016.1165735).

See also the [MetaAnalyser demo at shinyapps](https://chjackson.shinyapps.io/MetaAnalyser).

## Installation
Stable release on [CRAN](https://cran.r-project.org/web/packages/MetaAnalyser/index.html)
```r
install.packages("MetaAnalyser")
```

Development version on GitHub
```r
install.packages("devtools") # if devtools not already installed
devtools::install_github('rstudio/DT')
devtools::install_github('chjackson/MetaAnalyser')

# Test with
library(MetaAnalyser)
MetaAnalyser()
```
