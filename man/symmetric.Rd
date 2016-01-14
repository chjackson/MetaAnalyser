\name{symmetric}
\alias{symmetric}
\docType{data}
\title{
Artificially symmetric meta-analysis data
}
\description{
Artificial meta-analysis dataset with a symmetric pattern about the
pooled estimate.
}
\usage{data("symmetric")}
\format{
  A data frame with 13 observations on the following 3 variables.
  \describe{
    \item{\code{name}}{Study name, here simply a numeric vector from 1 to 13}
    \item{\code{est}}{Study-specific estimate}
    \item{\code{se}}{Standard error}
  }
}
\details{
    Used in this package to illustrate an idealised situation where there is no
    correlation between effect size and precision across studies.
  }
\examples{
\dontrun{MetaAnalyser(symmetric)}
}
\keyword{datasets}
