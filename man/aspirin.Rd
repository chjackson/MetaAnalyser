\name{aspirin}
\alias{aspirin}
\docType{data}
\title{
Aspirin meta-analysis data
}
\description{
  63 randomized controlled trials reported by Edwards et al. (1998) that
  each investigated the benefit of oral aspirin for pain relief.
}
\usage{data("aspirin")}
\format{
  A data frame with 63 observations on the following 3 variables.
  \describe{
    \item{\code{name}}{Study name}
    \item{\code{est}}{Study estimate: log-odds ratio for the proportion of patients in each arm
      who had at least a 50\% reduction in pain}
    \item{\code{se}}{Corresponding standard errors}
  }
}
\details{
This dataset is included in this package to demonstrate asymmetry in
meta-analysis, where smaller studies tend to show larger effect size
estimates, whereas larger studies tend to report more modest results.
}
\source{
Edwards, J. E. Oldman, A., Smith, L., Collins, S. L., Carol, D., Wiffen, P. J., McQuay,
H.J., and Moore, R.A. (1998) Single dose oral aspirin for acute pain. Cochrane Database
of Systematic Reviews, 4.
}
\examples{
\dontrun{MetaAnalyser(aspirin)}
}
\keyword{datasets}
