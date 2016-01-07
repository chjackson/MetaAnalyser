\name{colortable}
\alias{colortable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colortable(htmltab, css, style = "table-condensed table-bordered")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{htmltab}{
%%     ~~Describe \code{htmltab} here~~
}
  \item{css}{
%%     ~~Describe \code{css} here~~
}
  \item{style}{
%%     ~~Describe \code{style} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (htmltab, css, style = "table-condensed table-bordered") 
{
    tmp <- str_split(htmltab, "\n")[[1]]
    CSSid <- gsub("\\{.+", "", css)
    CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
    CSSidPaste <- gsub("#", "", CSSid)
    CSSid2 <- paste(" ", CSSid, sep = "")
    ids <- paste0("<td id='", CSSidPaste, "'")
    for (i in 1:length(CSSid)) {
        locations <- grep(CSSid[i], tmp)
        tmp[locations] <- gsub("<td", ids[i], tmp[locations])
        tmp[locations] <- gsub(CSSid2[i], "", tmp[locations], 
            fixed = TRUE)
    }
    htmltab <- paste(tmp, collapse = "\n")
    Encoding(htmltab) <- "UTF-8"
    list(tags$style(type = "text/css", paste(css, collapse = "\n")), 
        tags$script(sprintf("$( \"table\" ).addClass( \"table \%s\" );", 
            style)), HTML(htmltab))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
