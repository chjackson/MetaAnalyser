#' Heterogeneity standard deviation in meta-analysis
#'
#' Random effects standard deviation using the classic DerSimonian & Laird formula.
#'
#' @inheritParams MetaAnalyser
#'
#' @return Estimated random effects standard deviation
#'
#' @examples
#' resd_dsl(magnesium)
#'

resd_dsl <- function(dat){
    dat <- validate_data(dat)
    with(dat, {
        wtfe <- 1 / se^2
        a.fixef <- sum(wtfe*est/sum(wtfe))
        a.fixefse <- 1/sqrt(sum(wtfe))
        chisq <- sum(wtfe*(est - a.fixef)^2)
        k <- length(est)
        tausq <- max(0, (chisq - (k - 1)) / (sum(wtfe) - sum(wtfe^2) / sum(wtfe)))
        sqrt(tausq)
    } )
}

#' Meta-analysis summary statistics
#'
#' Compute meta-analysis weights and corresponding pooled estimates given a set of estimates and standard errors.  Weights are simply defined by the inverse variance, where the variance is the sum of the study-specific and random effects variance.
#'
#' @inheritParams MetaAnalyser
#'
#' @param resd Random effects standard deviation.  Set \code{resd=0} for a fixed effects meta-analysis.  If \code{resd} is omitted, a random effects meta-analysis is performed using the typical DerSimonian and Laird method to obtain the standard deviation (\code{\link{resd_dsl}}).
#'
#' @param egger Set to \code{TRUE} to perform Egger correction.
#'
#' @return A list with the following components:
#'
#' \item{est}{Original study-specific estimates (if \code{egger=FALSE}) or Egger-corrected version of these (if \code{egger=TRUE}).}
#'
#' \item{pool}{Pooled estimate}
#'
#' \item{poolse}{Pooled standard error}
#'
#' \item{poolci}{Pooled 95\% confidence interval}
#'
#' \item{pwtfe}{Weights for fixed effects model, normalised to sum to 1}
#'
#' \item{pwtre}{Weights for desired random effects standard deviation, normalised to sum to 1}
#'
#' @export metasumm
#' @export resd_dsl
#' @importFrom stats lm qnorm qt

metasumm <- function(dat, resd, egger=FALSE){
    est <- dat$est
    se <- dat$se
    if (missing(resd)) resd <- resd_dsl(dat)
    wtfe <- 1 / se^2
    poolfix <- sum(wtfe * est / sum(wtfe))
    tausq <- resd^2
    wtre <- 1 / (se^2 + tausq)
    if (egger) {
        egg <- eggersumm(est, se)
        est <- egg$ystar
        pool <- egg$pool
        poolse <- egg$poolse
        poolci <- egg$poolci
    } else {
        pool <- sum(wtre * est / sum(wtre))
        poolse <- 1/sqrt(sum(wtre))
        poolci <- pool + qnorm(c(0.025, 0.975))*poolse
    }
    ret <- list(est=est, pool=pool, poolse=poolse, poolci=poolci,
                pwtfe=wtfe/sum(wtfe), pwtre=wtre/sum(wtre))
    ret$Q <- sum(wtfe*(est - poolfix)^2)
    df <- length(est) - 1
    ret$Isq <- max(0, (ret$Q - df)/ret$Q)
    ret
}

## Egger regression
eggersumm <- function(est, se){
    z1 <- est/se
    z2 <- 1/se
    egger.lm <- summary(lm(z1 ~ z2))$coef
    alpha <- egger.lm["(Intercept)","Estimate"]
    ystar <- est - alpha*se
    df <- length(est) - 2
    t <- abs(qt(0.025, df))
    pool <- egger.lm["z2","Estimate"]
    poolse <- egger.lm["z2","Std. Error"]
    poolci <- pool + c(-t, t)*poolse
    list(ystar=ystar, pool=pool, poolse=poolse, poolci=poolci)
}
