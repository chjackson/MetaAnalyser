\name{magnesium}
\alias{magnesium}
\docType{data}
\title{
Magnesium and myocardial infarction meta-analysis data
}
\description{
8 randomised trials assessing the use of magnesium to treat myocardial
infarction, previously analysed by Higgins and Spiegelhalter (2002).
}
\usage{data("magnesium")}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{name}}{Study name}
    \item{\code{est}}{Log odds ratio of death (magnesium versus control)}
    \item{\code{se}}{Standard error for the log odds ratio}
  }
}
\source{
Higgins, J. P., & Spiegelhalter, D. J. (2002). Being sceptical about meta-analyses: a Bayesian perspective on magnesium trials in myocardial infarction. International Journal of Epidemiology, 31(1), 96-104.
}
\examples{
\dontrun{MetaAnalyser(magnesium)}
}
\keyword{datasets}
