\name{catheter}
\alias{catheter}
\docType{data}
\title{
Meta-analysis of antibacterial catheter coating 
}
\description{
Data on the effectiveness of silver sulfadiazine coating on venous
catheters for preventing bacterial colonisation of the catheter
and bloodstream infection.  A modified version of the data provided
by the \pkg{rmeta} package, excluding four small or uninformative studies.
}
\usage{data("catheter")}
\format{
  A data frame with 11 observations on the following 3 variables.
  \describe{
    \item{\code{name}}{Study name}
    \item{\code{est}}{Log odds ratio of bacteria colonisation (treatment
    compared to control)}
    \item{\code{se}}{Corresponding standard error}
  }
}
\details{
The Appavi, Pemberton, Logghe and Bach (a) studies are excluded.
The data here are produced from the source numerators and denominators using the
\code{meta.MH} method in \pkg{rmeta}.
}
\source{
     Veenstra D et al (1998) "Efficacy of Antiseptic Impregnated
     Central Venous Catheters in Preventing Nosocomial Infections: A
     Meta-analysis" JAMA 281:261-267
}
\references{
  The \pkg{rmeta} package (Lumley, 2012).
}
\examples{
\dontrun{
MetaAnalyser(catheter)
}
}
\keyword{datasets}
