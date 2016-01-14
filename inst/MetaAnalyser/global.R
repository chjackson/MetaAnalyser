library(shiny)
library(ggvis)
library(DT) # for data table
library(MetaAnalyser) # for example data and meta-analysis summary statistics

#options(warn=2)
#options(shiny.error=recover)

## Global variable containing studies added by the user on top of current data
globals <- list(
    newdata=NULL,
    pool=0  # current pooled estimate: used for tilt angle
)
