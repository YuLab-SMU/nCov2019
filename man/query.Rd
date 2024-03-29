% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{query}
\alias{query}
\title{Query COVID-19 related statistic data}
\usage{
query()
}
\value{
result contains 5 types of data:
\itemize{
\item global: The global overall summary statistic
\item latest: The global latest statistic for all countries
\item historical: The historical statistic for all countries
\item vaccine: The current vaccine development progress
\item therapeutics: The current therapeutics development progress
}
}
\description{
The main function for query nCov2019 related statistic data,
}
\examples{
\dontrun{
res <- query()
names(res)
}
}
