##' @method print global_summary
##' @export
print.global_summary <- function(x, ...) {
  cat("Gloabl total ", x$table$cases, " cases; and ", x$table$deaths," deaths" )
  cat("\nGloabl total affect country or areas:", x$table$affectedCountries)
  cat("\nGloabl total recovered cases:", x$table$todayRecovered)
  cat("\nlast update:", x$table$updated, "\n")
}

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
