library(shiny)
library(tidyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)
library(nCov2019)

get_latest_data <- getFromNamespace("get_latest_data", "nCov2019")
get_global_data <- getFromNamespace("get_global_data", "nCov2019")
get_history_data <- getFromNamespace("get_history_data", "nCov2019")
get_vaccine_data <- getFromNamespace("get_vaccine_data", "nCov2019")
get_therapeutics_data <- getFromNamespace("get_therapeutics_data", "nCov2019")

ui <- dashboardPage(
  dashboardHeader(title = "nCov2019 Dashboard"),
  dashboardSidebar(
  
    # choose country
    selectizeInput(
      'country', 'Choose Country', choices = NULL,
      options = list(
        placeholder = 'Choose Country',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),

    # choose province 
    selectizeInput('province', 'Choose Province', 
                        choices = c("Select Province" =  NULL  ),
                        options = list( placeholder = 'Province')),
                    tags$p(""),

    tags$div(
    tags$p("Download chosen data"),
    downloadButton('dataDownload', 'Download'),style = "padding: 12px 15px 0px 15px"),
    br(),
    actionButton("reflashButton", "Reflash data")
    ),

  dashboardBody(
    tags$head(tags$style("
    .shiny-notification {position: fixed; top: 45% ;left: 50%}
    .logo, .main-sidebar {position: fixed }")), 
 
    # header summary
    fluidRow(
      valueBoxOutput(outputId="summary_confirm"),
      valueBoxOutput(outputId="summary_cure"),
      valueBoxOutput(outputId="summary_dead")
    ),  

    fluidRow(
        # data table
    shinydashboard::box(title = "Historical Data Table",
            solidHeader = T,
            width = 4,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("data_table")), 
            style = "font-size: 70%;"),

        # line plot
    shinydashboard::box(title = "Cumulative Curve", solidHeader = T,
        width = 8, collapsible = T,
        shinycssloaders::withSpinner(plotlyOutput("line_plot")))
    ),  

    fluidRow(
      tabBox(
        width=12,
      title = "",
      selected = "Global Statistics",
      tabPanel("Global Statistics", 
        # choose type
        #selectizeInput(
        #    'type', label = "Choose Global Statistics", choices = c("cases","deaths","recovered","active","todayCases", "todayDeaths","todayRecovered","population","tests"),
        #        options = list(create = TRUE)
        #    ), 
        # choose date
        dateInput('date', label = 'Date:', min = '2020-01-22', value =NULL),
        shinycssloaders::withSpinner(
        plotlyOutput("Global_plot",height = '600', width = 'auto')
        )),
      tabPanel("Vaccine Statisics", 
        shinydashboard::box(
            width = 12,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("vaccine_table")), 
            style = "font-size: 70%;")),
      tabPanel("Therapeutics Statisics", 
        shinydashboard::box(
            width = 12,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("therapeutics_table")), 
            style = "font-size: 70%;")),
      tabPanel("Medical Summary Table", 
        shinydashboard::box(title = "current therapeutics candidates ",
            width = 6,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("Summary_table1")), 
            style = "font-size: 70%;"),
        shinydashboard::box(title = "current vaccine candidates",
            width = 6,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("Summary_table2")), 
            style = "font-size: 70%;")
      ),
      tabPanel("Active per Million", plotlyOutput("active_plot")),
      tabPanel("Deaths per Million", plotlyOutput("Mortality_plot")),
      tabPanel("Cor Plot", 
          # choose type
        tags$div(style = "display: flex;",
        selectizeInput( 'cor_type1', label = "Variable X", choices = NULL,options = list(create = TRUE)), 
        selectizeInput( 'cor_type2', label = "Variable Y", choices = NULL,options = list(create = TRUE))), 
        plotlyOutput("cor_plot")),
      tabPanel("Daily Increase Curve", 
            selectizeInput('country_list2', label = "Click or input countries to plot curve", 
            choices = NULL,multiple = TRUE,options = list(create = TRUE,multiple = TRUE)), 
            plotlyOutput("wave_plot"))  
    )
    ) # end row
  ) # end dashboard body 

) # end UI

server <- function(input, output, session, ...) {
    # query and reflash data 
    reflash <- function(){
            withProgress({
            incProgress(message = "Querying the lastest data")
            lastest_data <- get_latest_data()
            incProgress(message = "Querying the global summary data")
            global_data <- get_global_data()
            incProgress( message = "Querying the historical data")
            historical_data <- get_history_data()
            incProgress(  message = "Querying the vaccine data")
            vaccine_data <- get_vaccine_data()
            incProgress( message = "Querying the therapeutics data")
            therapeutics_data <- get_therapeutics_data()
            res = list(lastest=lastest_data,
                    global=global_data,
                    historical=historical_data,
                    vaccine=vaccine_data,
                    therapeutics=therapeutics_data)
            return(res)
            })
            return(res)
            }
        res <- reflash()
        lastest_data=res$lastest
        global_data=res$global
        historical_data=res$historical
        vaccine_data=res$vaccine
        therapeutics_data=res$therapeutics

    # update country list
    country_list <- dplyr::filter(lastest_data$table, updated == lastest_data$time) %>% 
                            arrange(desc(cases)) %>% .$country
    updateSelectizeInput(session, 'country', choices = country_list, server = TRUE)
    
    t = historical_data$time
    
    # update province list
    observe({
        province_list <- unique(subset(historical_data$province, country == input$country)$province)
        if (length(province_list) > 0) {
            updateSelectInput(session, "province", choices = c("All",province_list))
        } else {
            updateSelectInput(session, "province", choices = c("--",province_list))
        }
        
    })
    
    # update cor_type list
    observe({
        cor_type_list <- colnames(lastest_data$detail) 
        cor_type_list <- cor_type_list[!cor_type_list %in% c("updated","country","countryInfo")]
        updateSelectInput(session, "cor_type1", 
            choices = cor_type_list)
        updateSelectInput(session, "cor_type2", 
            choices = cor_type_list[c(2,1,3:length(cor_type_list))])
    })

    # update country list2
    country_list <- unique(historical_data$table$country ) 
    updateSelectizeInput(session, 'country_list2', choices = country_list, server = TRUE)

    updateDateInput(session, 'date', value = t, max=t)
    # prepare the table content
    df <- reactive({
        if ( input$province == "All" | input$province == "--"  ) {
            x = subset(historical_data$table, country == input$country)
        }
        else {
            x = subset(historical_data$province, province == input$province)
        }
        x = x[,c("date","cases","deaths","recovered")]
        return(x)
    })
    historical_data$table %>%
    group_by(country) %>%
    arrange(country,date) %>%
    mutate(diff = cases -  dplyr::lag(cases, default =  dplyr::first(cases))) -> a
  # output data table
    output$data_table = DT::renderDataTable({
        validate(need(input$country != "", "Loading"))
        df()
    },rownames = FALSE )

 # output header summary 
    output$summary_confirm <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$date == t),]$cases, " Cases"), 
                    t, icon = icon("virus"), color = "yellow")
    })

    output$summary_cure <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$date == t),]$recovered, " Recovered"), 
                    t, icon = icon("hospital"), color = "navy")
    })

    output$summary_dead <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$date == t),]$deaths, " Deaths"), 
                    t, icon = icon("skull-crossbones"), color = "maroon")
})

# Growth Curve
    output$line_plot <- renderPlotly({
        validate(need(input$country != "", "Loading"))
        x = gather(df(), curve, count, -date)
        p = ggplot(x, aes(date, log2(count), color = curve, Counts=count, Type=curve )) +
            geom_point() + geom_line() + xlab(NULL) + ylab("Log2 of count") +
            scale_color_manual(values=c("#f39c12", "#d81b60", "#000080")) +
            theme_bw() + 
            theme(legend.position = "none") +
                theme(axis.text = element_text(angle = 15, hjust = 1)) +
                scale_x_date(date_labels = "%Y-%m-%d")
        ggplotly(p,tooltip=c("x","Counts","Type"))
    })

# data download

    output$dataDownload <- downloadHandler(
      filename = function() {paste0("coronavirus_histrical_",t,".tsv")},
      content = function(file) {
        # issues with Chinese characters solved
        # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
        con <- file(file, open = "w+", encoding = "native.enc")
        df <- df()
        df$country = input$country
        df$province = input$province
        writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
        for(i in 1:nrow( df) )
          #write line by line 
          writeLines( paste( as.character(df[i,]), collapse = "\t"), con = con, useBytes = TRUE)
        close(con)
      }
    )

# bottom panel plots


    output$Global_plot <- renderPlotly({
        #validate(need(input$type != "", "Loading"))
        
        geoINFO = data.frame(
            country = lastest_data$detail$country, 
            ISO3 = lastest_data$detail$countryInfo$iso3,
            long = lastest_data$detail$countryInfo$long,
            lat = lastest_data$detail$countryInfo$lat,
            population = lastest_data$detail$population
        )
        df = subset(historical_data$table ,date == input$date)
        df2 = merge(df, geoINFO, by='country')
        #lastest_data$detail$ISO2 = lastest_data$detail$countryInfo$iso2
        #lastest_data$detail$lat = lastest_data$detail$countryInfo$lat
        #lastest_data$detail$long = lastest_data$detail$countryInfo$long
        #lastest_data$detail$ISO3 = lastest_data$detail$countryInfo$iso3
        #df2 <- lastest_data$detail

        g <- list(
        scope = 'world',
        showland = TRUE,
        showcountries = TRUE,
        landcolor = toRGB("gray95"),
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("black")
        )

        # specify map projection/options
        l <- list(color = toRGB("grey95"), width = 0.5)
        g2 <- list(
        showframe = TRUE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator')
        )
  
        fig <- plot_geo(df2,
                hoverinfo = 'text',
                text = ~paste(
                            '</br> Region: ', country,
                            '</br> Cases: ', cases,
                            '</br> Deaths: ', deaths,
                            '</br> Population: ', population,
                            '</br> </br> Date: ', date 
                            )
         )
        fig <- fig %>% add_trace(
#        z = as.formula(paste0("~`", input$type, "`")) , 
#        color = as.formula(paste0("~`", input$type, "`")), colors = 'Reds', 
#        hoverinfo= as.formula(paste0("~`", input$type, "`")),
        z = ~cases,
        color = ~cases,
        colors = 'Reds',
        locations = ~ISO3
        )
        fig <- fig %>% colorbar(title = "cases" )
        fig <- fig %>% plotly::layout(
        title = '  ',
        geo = g
        ) 
})

    output$vaccine_table = DT::renderDataTable({
        validate(need( exists("vaccine_data"), "Loading"))
        vaccine_data$table[,!colnames(vaccine_data$table) %in% "details"]
    },rownames = FALSE ) 

    output$therapeutics_table = DT::renderDataTable({
        validate(need( exists("therapeutics_data"), "Loading"))
        therapeutics_data$table[,!colnames(therapeutics_data$table) %in% c("details")]
    },rownames = FALSE ) 

    output$Summary_table1 = DT::renderDataTable({
        validate(need( exists("vaccine_data"), "Loading"))
        therapeutics_data$summary
    },rownames = FALSE ) 
    output$Summary_table2 = DT::renderDataTable({
        validate(need( exists("therapeutics_data"), "Loading"))
        vaccine_data$summary
    },rownames = FALSE ) 

    output$active_plot <- renderPlotly({
        validate(need( exists("lastest_data"), "Loading"))
        df = lastest_data$detail
        df = df[order(df$activePerOneMillion,decreasing = T),] 
        p <- ggplot(df, aes(country,activePerOneMillion)) + 
            geom_col(color="firebrick")  + scale_x_discrete(limits= df$country) +
            geom_hline(yintercept = mean(df$activePerOneMillion)) + 
            theme_minimal() + ylab("Active cases per million population") +
            theme(  axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    legend.position = 'none')
        ggplotly(p, tooltip = c("country","activePerOneMillion"))
    })

    output$Mortality_plot <- renderPlotly({
        validate(need( exists("lastest_data"), "Loading"))
        df = lastest_data$detail
        df = df[order(df$deathsPerOneMillion,decreasing = T),] 
        p <- ggplot(df, aes(country,deathsPerOneMillion)) + 
            geom_col(color="firebrick")  + scale_x_discrete(limits= df$country) +
            geom_hline(yintercept = mean(df$deathsPerOneMillion)) + 
            theme_minimal() + ylab('Mortality per million population') +
            theme(  axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    legend.position = 'none')
        ggplotly(p, tooltip = c("country","deathsPerOneMillion"))
    })

    # cor_plot
    output$cor_plot <- renderPlotly({
        validate(need( exists("lastest_data"), "Loading"))
        df = lastest_data$detail 
        x = input$cor_type1
        y = input$cor_type2
        p = ggplot(df, aes_string(x,y, color="country")) + 
            geom_jitter() +  guides(color = FALSE )   +
            theme_minimal() +  
            labs(subtitle="The size of the dots in the graph corresponds to the number of patients diagnosed today") +
            theme(  axis.text=element_blank(),
                    axis.ticks=element_blank(),
                    legend.position = 'none' )    
})
    # wave_plot
    output$wave_plot <- renderPlotly({
        validate(need(input$country_list2, "Pleas choose some countries"))
        tmp = subset(a, country %in% input$country_list2)
        tmp$Log2Increase = log2(tmp$diff + 1) 
        tmp$Increase = tmp$diff
        p <- ggplot(tmp,aes(date, Log2Increase,color=country, Increase = Increase)) + geom_line()  +
                    labs(y="Daily increase cases(log2 scale)") + 
                    theme(axis.text = element_text(angle = 15, hjust = 1)) +
                    scale_x_date(date_labels = "%Y-%m-%d") + theme_minimal()
        ggplotly(p, tooltip = c("country","date", "Increase"))
})
    ### reflash button
    observeEvent(input$reflashButton, {
        res <- reflash()
        lastest_data=res$lastest
        global_data=res$global
        historical_data=res$historical
        vaccine_data=res$vaccine
        therapeutics_data=res$therapeutics
    })
### end
}

shinyApp(ui = ui, server = server)
