##' @title Shiny app
##' @rdname dashboard
##' @description a shiny app
##' @importFrom downloader download
##' @export 
open_dashboard <- function() {
        package_need <- c('scales','shinycssloaders','shinydashboard', 'plotly', 'ggplot2', 
                'shiny', 'shinyBS', 'DT', 'tidyr', 'reshape2')
        package_no <-  package_need[!is.installed(package_need)]
        
        if(length(package_no) != 0 ) {
            pa <- paste0(package_no,"\t")
            messages2 <- "Running this shiny app requires some additional R packages, download them? (Y/N): "
            ## button2 <- tcltk::tkmessageBox(title='Message', message=messages2, type='yesno')
            ## button2 <- tcltk::tclvalue(button2)
            button2 <- toupper(readline(prompt = messages2))
            if(button2 == 'N'){
                stop("Running this shiny app requires some additional R packages,",
                    pa, ",please install them")
                } else {
                message("Running this shiny app requires some additional R packages,",
                    pa, ",this will take some time")
                # install packages from CRAN
                ## sapply(package_no, utils::install.packages)
                utils::install.packages(package_no)
            }
         }
        pos <- 1
        envir <- as.environment(pos)
        if (!exists("nCov2019Env", envir = .GlobalEnv)) {
            assign("nCov2019Env", new.env(), envir = envir)
        }
        nCov2019Env <- get("nCov2019Env", envir = .GlobalEnv)
        options(nCov2019_dashboard = TRUE)

    # run shinyApp
    shiny::runApp(appDir = system.file("shinyapp" ,package="nCov2019") )
}

##' @rdname dashboard
##' @export 
dashboard <- open_dashboard

is.installed <- function(packages) {
    vapply(packages, function(pkg) {
        system.file(package = pkg) != ""
    }, logical(1))
}




