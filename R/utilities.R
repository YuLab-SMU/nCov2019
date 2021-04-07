##' @method [ nCov2019History
##' @export
`[.nCov2019History` <- function(x, Country, Province, ...) {
  country <- province <- NULL
  if("Global" %in% Country  || "global" %in% Country){
    res = x$table
  }
  else if (missing(Province)){
    res = subset(x$table, country  %in% Country)
  } 
  else {
    res = subset(x$province, country  %in% Country & province  %in% Province)
  }
  return(res)
}

##' @method [ nCov2019
##' @export
`[.nCov2019` <- function(x, Country, ...) {
  country <- NULL
  if("Global" %in%  Country || "global" %in% Country){
    res = x$table
  } else {res = subset(x$table, country  %in% Country)}
  return(res)
}

##' @method [ vaccine_therapeutics
##' @export
`[.vaccine_therapeutics` <- function(x, ID, ...) {
  id <- NULL
  if("All" %in% ID  || "all" %in% ID){
    res = x$table[,!colnames(x$table) %in% "details"]
  } else {res = subset(x$table, id %in% ID)[,"details"]}
  return(res)
}

##' @method summary vaccine_therapeutics
##' @export
summary.vaccine_therapeutics <- function(object, ...){
  return(object$summary)
}


##' @method summary global_summary
##' @export
summary.global_summary <- function(object, ...) {
  x <- object
  cat("Gloabl total ", x$cases, " cases; and ", x$deaths," deaths" )
  cat("\nGloabl total affect country or areas:", x$affectedCountries)
  cat("\nGloabl total recovered cases:", x$todayRecovered)
  cat("\nlast update:", x$updated, "\n")
}


##' @title  convert 
##' @rdname convert
##' @description Convert users' own data into class of nCov2019History data. Then it could be used in nCov2019.
##' @param data users' own data, it should contain these 6 column: "country","province","date","cases","deaths","recovered".
##' @return a 'nCov2019History' object
##' @export
convert <- function(data) {
  if (sum(c("country","province","date","cases","deaths","recovered") %in% colnames(data)) != 6){
    stop('Input data should contain these 6 column: "country","province","date","cases","deaths","recovered"')
  }
  
  cases_table <- data[,c("country","date","cases","deaths","recovered")]
  Pcases_table <- data[,c("country","province","date","cases","deaths","recovered")]
  
  res = list(
    table = cases_table[order(cases_table$country, cases_table$date), ],
    province = Pcases_table[order(Pcases_table$country, Pcases_table$date), ],
    time = as.character(max(cases_table$date))
  )
  class(res) = "nCov2019History"
  return(res)
}

##' @importFrom stats aggregate
clean_data <- function(data, object, by = "country") {
  cases_table = data.frame(data$timeline[[object]],check.names = F)
  if (by == "country") {
    by <- list(data$country)
    id <- c("Group.1")
  } else {
    by <- list(data$country,data$province)
    id <- c("Group.1", "Group.2")
  }
  tmp = aggregate(x = cases_table,by = by, FUN = sum)
  return(reshape2::melt(tmp, id = id, value.name = object))
}
