
##' @title plot.nCov2019
##' @method plot nCov2019
##' @description plot map with ggplots, it is the core of plot.nCov2019 and Plot.nCov2019History.
##' @param x data for plot, it should be class of nCov2019 or nCov2019Hisory.
##' @param region If Global or a specified region.
##' @param continuous_scale logical, Whether to use continuous fill color, 
##' if TRUE(the default), use continuous type, otherwise use discrete type.
##' @param palette If a string, will use that named palette. If a number, 
##' will index into the list of palettes of appropriate type.
##' The list of available palettes can found in the Palettes section.
##' @param date Specify the date.
##' @param title Title of the map.
##' @param type Specify the type of Statistics.
##' @param ... Additional parameters.
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 map_data
##' @importFrom ggplot2 geom_map
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 coord_equal
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 aes_string
##' @export
plot.nCov2019 <- function(x, region = "Global", continuous_scale=FALSE, palette = "Reds",date=NULL, title="COVID19", type="cases",... ) {
    country <- NULL
    if (class(x) == "nCov2019"){
    type_list = c("cases","deaths","recovered","active","todayCases","todayDeaths","todayRecovered","population","tests")
    } else if(class(x) == "nCov2019History"){type_list = c("cases","deaths","recovered","active")}
    
    if (!type %in% type_list ) {
        msg <- paste("`type` should be one of below:\n",
                        paste(type_list, collapse=","), "\n")
        stop(msg)
    }
    
    if (!is.null(date)) {
        caption <- paste("accessed date:", date)
    } 
    else if("time" %in% names(x)) {
        date = x$time
        caption <- paste("accessed date:",  date)  
    } 
    else {
        caption  <- NA
    }

    # get the subset of one day for historical_data
    if (class(x) == "nCov2019History"){
        time = date
        dt = subset(x$table, date == time)
    } else{
        dt = x$table
    }

    # caculate the summary data
    if (region != "Global") {
        d <- subset(dt[c("country", type)], country == region)
        total = d[[type]][1]
        } else {
            d <- dt[c("country", type)]
            total <- sum(d[[type]])        
        }
   
    subtitle = paste(region, type, ":", total)
    if (region == "China") {
      subtitle = paste("China[Mainland]", type, ":", total)
    }
    
    if (region == "Global") region <- "."
    world <- map_data('world', region = region)
    world <- world[world$region != "Antarctica", ]
    w <- merge(world, d, by.x='region', by.y='country', all.x=T)
    w[[type]][is.na(w[[type]])] = 0
    w <- w[order(w$order),]
    p <- ggplot(w, aes_(~long, ~lat)) + 
        coord_equal() +
        theme_minimal(base_size = 14) +
        xlab(NULL) + ylab(NULL) +
        labs(title = title, 
            subtitle = subtitle,
            caption= caption )
    discrete_type = paste0(type,"2") 
    w[[discrete_type]] = cut(w[[type]], discrete_breaks,
             include.lowest = T, right=F)

    if (continuous_scale) {
        if (length(unique(w[[type]])) == 1) {
            col <- RColorBrewer::brewer.pal(3, palette)[3]
            p1 <- p +  
                geom_map(aes_(~long, ~lat, map_id = ~region, group=~group), 
                         map=w, data=w, colour='grey', fill = col)
            
        } else {
            p1 <- p +  
                geom_map(aes_string("long", "lat", map_id = "region", group="group", fill=type), 
                         map=w, data=w, colour='grey')  +
                        fill_scale_continuous(palette)
        }
    } else {
        p1 <- p + 
            geom_map(aes_string("long", "lat", map_id = "region", group="group", fill=discrete_type), 
                    map=w, data=w, colour='grey') + 
                    fill_scale_discrete(palette) 
    }    


    #p1 <- p + 
    #  geom_map(aes_(~long, ~lat, map_id=~region, group=~group, fill=~cases2), map=w, data=w, colour='grey') 
                
    return(p1 )
}

discrete_breaks <- c(1,10,100, 10^3,10^4, 10^5, 10^6,10^7,10^8, 10^9, 10^10)

fill_scale_discrete <- function(palette = "Reds") {
    ggplot2::scale_fill_brewer(palette=palette, name=' ', 
            na.translate = FALSE, 
            breaks = c('[0,10)','[10,100)', '[100,1e+03)', '[1e+03,1e+04)', '[1e+04,1e+05)', 
                      '[1e+05,1e+06)', '[1e+06,1e+07)','[1e+07,1e+08)','[1e+08,1e+09)',
                      '[1e+09,1e+10)'),
            labels = c(expression(0-10^1), 
                       expression(10^1-10^2), 
                       expression(10^2-10^3), 
                       expression(10^3-10^4), 
                       expression(10^4-10^5), 
                       expression(10^5-10^6), 
                       expression(10^6-10^7), 
                       expression(10^7-10^8), 
                       expression(10^8-10^9),
                       expression(10^9-10^10)))
}

fill_scale_continuous <- function(palette = "Reds") {
    cols = RColorBrewer::brewer.pal(6, palette)
    breaks = c( 10, 10^2, 10^3, 10^4,10^5,10^6,10^7,10^8)
    labels = c( expression(10^1), 
           expression(10^2), 
           expression(10^3), 
           expression(10^4), 
           expression(10^5), 
           expression(10^6), 
           expression(10^7), 
           expression(10^8) )
    ggplot2::scale_fill_gradient(low=cols[1], high=cols[6],
                na.value='white', trans='log',
                breaks=breaks,labels = labels)
}


##' @method plot nCov2019History
##' @param from start date to plot
##' @param to end date to plot. Both from and to should be specify, otherwise they will be ignored.
##' If both from and to are specify, an animation will be created.
##' @param width width of the plot, only works for animation
##' @param height height of the plot, only works for animation
##' @param fps fps of the animation, only works for animation
##' @export
plot.nCov2019History <- function(x, region="Global", 
                                 continuous_scale = TRUE, 
                                 palette = "Reds", date=NULL,
                                 from = NULL, to = NULL, 
                                 width = 600, height = 600, filename = "nCov2019.gif", fps=2, type, ...) {
    if (is.null(from) || is.null(to)) {
         
        p <- plot.nCov2019(x = x, date=date,
                           region = region,type=type,
                           continuous_scale = continuous_scale,
                           palette = palette, ...)
        return(p)
    }

    from <- as.Date(from)
    to <- as.Date(to)
    d <- seq(from, to, by = 1)

    out <- lapply(d, function(date){
 
        p <- plot.nCov2019(x = x,
                           region = region, type=type,  
                           continuous_scale = continuous_scale,
                           palette = palette, date = date, ...)
    })

    leg <- cowplot::get_legend(out[[length(out)]])
    out <- lapply(out, function(g) {
        ## ggplotify::as.ggplot(g + ggplot2::theme(legend.position="none")) + 
        ##     ggimage::geom_subview(subview = leg, x=.9, y=.2)
        cowplot::plot_grid(g + ggplot2::theme(legend.position="none"),
                           leg, rel_widths = c(1, .3))
    })


    img <- magick::image_graph(600, 600, res = 96)
    invisible(lapply(out, function(p) suppressWarnings(print(p))))
    grDevices::dev.off()

    animation <- magick::image_animate(img, fps = fps)
    msg <- paste0("A gif, ", filename, ", was generated in current directory\n")
    message(msg)
    magick::image_write(animation, filename)
    invisible(animation)
}