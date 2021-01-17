##' @method print nCov2019
##' @export
print.nCov2019 <- function(x, ...) {
  cat("last update:", x$time, "\n")
}

##' @method print nCov2019History
##' @export
print.nCov2019History <- print.nCov2019

##' @method print vaccine_therapeutics
##' @export
print.vaccine_therapeutics <- function(x, ...) {
  cat("Total Candidates Programs :", x$totalCandidates,"\n")
}
