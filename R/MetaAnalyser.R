#' The Meta-Analyser
#'
#' An interactive application to visualise meta-analysis data as a physical weighing machine
#'
#' @param dat Meta-analysis data. This should be a data frame with three columns, called "name", "est" and "se" giving the study name, study-specific parameter estimates and corresponding standard errors respectively.  If omitted, the app is launched with the default \code{\link{magnesium}} data.
#'
#' @param rstudio The default of FALSE opens the app in the system default web browser.
#'
#' @return None
#'
#' @details Opens a web browser with the interactive application
#'
#' @examples
#' \dontrun{MetaAnalyser(magnesium)}
#'
#' @references J. Bowden and C. Jackson "Weighing evidence with the Meta-Analyser"
#'

MetaAnalyser <- function(dat, rstudio=FALSE){
    if (!missing(dat)){
        validate_data(dat)
        .dat_env$userdata <- dat
        .dat_env$userdataname <- deparse(as.list(match.call())$dat)
    } else {
        .dat_env$userdata <- NULL
        .dat_env$userdataname <- "magnesium"
    }
    on.exit(.dat_env$userdata <- .dat_env$userdataname <- NULL, add = TRUE)

    launch.browser <- if (!rstudio)
                          TRUE else getOption("shiny.launch.browser", interactive())
    ## R code for shiny app is in inst/MetaAnalyser in source package,
    ## or in MetaAnalyser in binary package
    shiny::runApp(system.file("MetaAnalyser", package = "MetaAnalyser"),
                  launch.browser = launch.browser)
}

validate_data <- function(dat){
    if (!is.data.frame(dat)){
        if (is.matrix(dat))
            dat <- as.data.frame(dat)
        else stop("\"dat\" should be a data frame")
    }
    if (ncol(dat) < 3)
        stop("\"dat\" should be a data frame with 3 columns, found ", ncol(dat), " columns")
    names(dat)[1:3] <- c("name","est","se")
    if (!is.numeric(dat$est)) stop("second column of \"dat\", giving estimate, should be numeric")
    if (!is.numeric(dat$se)) stop("third column of \"dat\", giving standard error, should be numeric")
}
