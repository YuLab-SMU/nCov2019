##' @method [ nCov2019History
##' @export
`[.nCov2019History` <- function(x, Country, Province, ...) {
  country <- province <- NULL
  if(Country == "Global" || Country == "global"){
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
  if(Country == "Global" || Country == "global"){
    res = x$table
  } else {res = subset(x$table, country  %in% Country)}
  return(res)
}

##' @method [ vaccine_therapeutics
##' @export
`[.vaccine_therapeutics` <- function(x, ID, ...) {
  id <- NULL
  if(ID == "All" || ID == "all"){
    res = x$table[,!colnames(x$table) %in% "details"]
  } else {res = subset(x$table, id %in% ID)[,"details"]}
  return(res)
}

##' @method summary vaccine_therapeutics
##' @export
summary.vaccine_therapeutics <- function(x, ...){
  return(x$summary)
}


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
