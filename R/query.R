#' @rdname query
#' @title Query COVID-19 related statistic data
#' @description The main function for query nCov2019 related statistic data,
#' @return result contains 5 types of data: 
#' * global: The global overall summary statistic
#' * latest: The global latest statistic for all countries
#' * historical: The historical statistic for all countries
#' * vaccine: The current vaccine development progress
#' * therapeutics: The current therapeutics development progress
#' @examples
#' \dontrun{
#' res <- query()
#' names(res)
#'}
#' @export
#' @md
query <- function(){
    message("Querying the latest data...")
    latest_data <- get_latest_data();print(latest_data)
    message("Querying the global data...")
    global_data <- get_global_data();summary(global_data)
    message("Querying the historical data...")
    historical_data <- get_history_data()
    message("Querying the vaccine data...")
    vaccine_data <- get_vaccine_data();print(vaccine_data)
    message("Querying the therapeutics data...")
    therapeutics_data <- get_therapeutics_data();print(therapeutics_data)
    message("Query finish, each time you can launch query() to reflash the data")
    res = list(latest=latest_data,
               global=global_data,
               historical=historical_data,
               vaccine=vaccine_data,
               therapeutics=therapeutics_data)
    return(res)
}

##' Query global latest statistic for all contries
##'
##' 
##' @title Query the latest data online
##' @return A 'nCov2019' object
##' @export
##' @author Guangchuang Yu
get_latest_data <- function() {
    url <- "https://disease.sh/v3/covid-19/countries?yesterday=1&twoDaysAgo=0&sort=todayCases"
    local <- file.path("local_storage","latest_data.json.gz")
    data <- dl(url,local)

    data$updated = sapply(data$updated, function(x){as.character(
            as.Date(as.POSIXct( x/1000, origin="1970-01-01")))} )
    res = list(
        table = data[c("country","cases","deaths","recovered","active","todayCases",
                    "todayDeaths", "todayRecovered","population","tests","updated")],
        detail = data[,!colnames(data) %in% c("countryInfo.flag")],
        time = max(data$updated)
    )
    class(res) = "nCov2019"
    return(res)
}



#' @title Query the global data online
#' @return A 'global_summary' object
#' @examples
#' \dontrun{
#' x <- get_global_data()
#'}
get_global_data <- function() {
    url <- "https://disease.sh/v3/covid-19/all?yesterday=false&twoDaysAgo=0"
    local <-  file.path("local_storage","global_data.json.gz")
    data <- dl(url,local)
    data$updated =  as.character(
            as.Date(as.POSIXct( data$updated/1000, origin="1970-01-01")))
    res = data.frame(data)
    class(res) = "global_summary"
    return(res)
}


#' @title Query the historical data online
#' @return A 'nCov2019History' object
#' @examples
#' \dontrun{
#' x <- get_history_data()
#'}
get_history_data <- function() {
    url <- "https://disease.sh/v3/covid-19/historical?lastdays=all"
    local <-  file.path("local_storage","historical_data.json")
    data <- dl(url,local)

    cases_table <- clean_data(data = data, object = "cases")
    deaths_table <- clean_data(data = data, object = "deaths")
    recovered_table <- clean_data(data = data, object = "recovered")
    cases_table$deaths = deaths_table$deaths
    cases_table$recovered = recovered_table$recovered
    colnames(cases_table)[1:2] = c("country","date")
    cases_table$date <- as.Date(format(as.Date(cases_table$date, "%m/%d/%y"), "%Y-%m-%d"))


    # `P` for province
    Pcases_table <- clean_data(data = data, object = "cases", by = "province")
    Pdeaths_table <- clean_data(data = data, object = "deaths", by = "province")
    Precovered_table <- clean_data(data = data, object = "recovered", by = "province")
    Pcases_table$deaths = Pdeaths_table$deaths
    Pcases_table$recovered = Precovered_table$recovered
    colnames(Pcases_table)[1:3] = c("country","province","date")
    Pcases_table$date <- as.Date(format(as.Date(Pcases_table$date, "%m/%d/%y"), "%Y-%m-%d"))

    res = list(
        table = cases_table[order(cases_table$country, cases_table$date), ],
        province = Pcases_table[order(Pcases_table$country, Pcases_table$date), ],
        time = as.character(max(cases_table$date))
    )
    class(res) = "nCov2019History"
    return(res)
}



#' @title Query the vaccine info online
#' @return A 'vaccine_therapeutics' object
#' @examples
#' \dontrun{
#' x <- get_vaccine_data()
#'}
get_vaccine_data <- function() {
    url <- "https://disease.sh/v3/covid-19/vaccine"
    local <-  file.path("local_storage","vaccine_data.json.gz")
    data <- dl(url,local)
    table <- data$data

    table$institutions = sapply(table$institutions,function(x){paste(x[[1]], collapse = "|")})
    table$sponsors = sapply(table$sponsors,function(x){paste(x[[1]], collapse = "|")})
    table$id = paste0("id",1:nrow(table))
    table = table[ c("id", colnames(table)[-ncol(table)]) ] # reorder
    res = list(
        table = table,
        summary = data$phases,
        totalCandidates = data$totalCandidates
    )
    class(res) = "vaccine_therapeutics"
    return(res)
}


#' @title Query the therapeutics info online
#' @return A 'vaccine_therapeutics' object
#' @examples
#' \dontrun{
#' x <- get_therapeutics_data()
#'}
get_therapeutics_data <- function() {
    url <- "https://disease.sh/v3/covid-19/therapeutics"
    local <-  file.path("local_storage","therapeutics_data.json.gz")
    data <- dl(url,local)
    table <- data$data

    table$tradeName = sapply(table$tradeName,function(x){paste(x[[1]], collapse = "|")})
    table$developerResearcher = sapply(table$developerResearcher,function(x){paste(x[[1]], collapse = "|")})
    table$sponsors = sapply(table$sponsors,function(x){paste(x[[1]], collapse = "|")})
    table$lastUpdate <- as.Date(format(as.Date(table$lastUpdate , "%m/%d/%y"), "%Y-%m-%d"))
    table$id = paste0("id",1:nrow(table))
    table = table[ c("id", colnames(table)[-ncol(table)]) ] # reorder
    res = list(
        table = table,
        summary = data$phases,
        totalCandidates = data$totalCandidates
    )
    class(res) = "vaccine_therapeutics"
    return(res)
}

#' @rdname get_nCov2019
#' @title Query the latest data
#' @description Query the latest data online;deprecated, 
##' use get_latest_data() in the further.
#' @return The latest statistic data
#' @examples
#' \dontrun{
#' x <- get_nCov2019()
#'}
#' @export
get_nCov2019  <- function(){
    message("`get_nCov2019()` has been deprecated and used `query()` instead of")
    message("Querying the latest data...")
    res <- get_latest_data()
    return(res)
}

#' @rdname load_nCov2019
#' @title Query the latest data
#' @description Query the historical data online;deprecated, 
##' use load_nCov2019() in the further.
#' @return The historical statistic data
#' @examples
#' \dontrun{
#' x <- load_nCov2019()
#'}
#' @export
load_nCov2019  <- function(){
    message("`load_nCov2019()` has been deprecated and used `query()` instead of.")
    message("Querying the historical data...")
    res <- get_history_data()
    return(res)
}


##' @importFrom utils download.file
dl <- function(url,local){
    tryCatch({
        d = tempfile()
        status <- download.file(url, quiet = T, destfile = d)
        data <- jsonlite::fromJSON(d)        
        },
    error= function(e) {
        message("Failed to query online data, please check the network connection.\n
        A local data  stored on 2021-01-11 will be loaded.")
        data <- jsonlite::fromJSON(system.file(local, package="nCov2019"))
    })
    return(data)
}
