#' The Meta-Analyser
#'
#' An interactive application to visualise meta-analysis data as a physical weighing machine
#'
#' @param dat Meta-analysis data. This should be a data frame with three columns, called "name", "est" and "se" giving the study name, study-specific parameter estimates and corresponding standard errors respectively.
#'
#' Numeric or character study names are permitted.  If the data frame has more than three columns, the first three are used.  If the first three columns are called "name", "est" and "se" in some order, they are re-ordered appropriately, otherwise they are re-named.
#'
#' @param rstudio The default of FALSE opens the app in the system default web browser.  If running RStudio and \code{rstudio=TRUE}, the app is opened in the RStudio built-in viewer.
#'
#' @return None
#'
#' @details Opens a web browser with the interactive application.
#'
#' If \code{dat} is omitted, the default \code{\link{magnesium}} dataset is used.
#'
#' \code{MetaAnalyzer} is an alias for \code{MetaAnalyser}.
#' 
#' @examples
#' \dontrun{MetaAnalyser(magnesium)}
#'
#' @references J. Bowden and C. Jackson "Weighing evidence with the Meta-Analyser" The American Statistician (2016) Available online, \url{http://dx.doi.org/10.1080/00031305.2016.1165735}
#'
#' @export MetaAnalyser
#' @export MetaAnalyzer
#' @importFrom DT renderDataTable dataTableProxy dataTableOutput
#' @importFrom shiny runApp
#' @import ggvis
#' 

MetaAnalyser <- function(dat, rstudio=FALSE){
    if (!missing(dat)){
        dat <- validate_data(dat)
        .dat_env$userdata <- dat
        .dat_env$userdataname <- deparse(as.list(match.call())$dat)
    } else {
        .dat_env$userdata <- NULL
        .dat_env$userdataname <- "magnesium"
    }
    on.exit(.dat_env$userdata <- .dat_env$userdataname <- NULL, add = TRUE)

    launch.browser <- if (!rstudio) TRUE else rstudioapi::viewer
    ## R code for shiny app is in inst/MetaAnalyser in source package,
    ## or in MetaAnalyser in binary package
    shiny::runApp(system.file("MetaAnalyser", package = "MetaAnalyser"),
                  launch.browser = launch.browser)
}

#' @rdname MetaAnalyser
MetaAnalyzer <- MetaAnalyser

validate_data <- function(dat){
    if (!is.data.frame(dat)){
        if (is.matrix(dat))
            dat <- as.data.frame(dat)
        else stop("\"dat\" should be a data frame")
    }
    if (ncol(dat) < 3)
        stop("\"dat\" should be a data frame with 3 columns, found ", ncol(dat), " columns")
    if (all(sort(names(dat)[1:3]) == c("est","name","se")))
        dat <- dat[,c("name","est","se")]
    else names(dat)[1:3] <- c("name","est","se")
    if (!is.numeric(dat$est)) stop("second column of \"dat\", giving estimate, should be numeric")
    if (!is.numeric(dat$se)) stop("third column of \"dat\", giving standard error, should be numeric")
    dat
}
