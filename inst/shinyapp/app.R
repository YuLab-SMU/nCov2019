library(shiny)
library(tidyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)

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

    # choose type
    selectizeInput(
        'type', label = "Global Statistics Variable", choices = c("cases","deaths","recovered","active","todayCases", "todayDeaths","todayRecovered","population","tests"),
            options = list(create = TRUE)
        ), 
    # choose type
    selectizeInput(
        'cor_type1', label = "Cor-Plot Variable X", choices = NULL,options = list(create = TRUE)
        ), 
    # choose type
    selectizeInput(
        'cor_type2', label = "Cor-Plot Variable Y", choices = NULL,options = list(create = TRUE)
        ), 
    # choose country_list2 for wave plot
    selectizeInput(
        'country_list2', label = "Click or Delete countries to plot daily increase curve", 
        choices = NULL,multiple = TRUE,options = list(create = TRUE,multiple = TRUE)
        ), 

    tags$div(
    tags$p("Download chosen data"),
    downloadButton('dataDownload', 'Download'),style = "padding: 12px 15px 0px 15px"),
    br(),
    actionButton("reflashButton", "Reflash data")
    ),

  dashboardBody(
    tags$head(tags$style(".shiny-notification {position: fixed; top: 45% ;left: 50%")), 
    # header summary
    fluidRow(
      valueBoxOutput(outputId="summary_confirm"),
      valueBoxOutput(outputId="summary_cure"),
      valueBoxOutput(outputId="summary_dead")
    ),  

    fluidRow(
        # data table
        box(title = "Historical Data Table",
            solidHeader = T,
            width = 4,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("data_table")), 
            style = "font-size: 70%;"),

        # line plot
        box(title = "Cumulative Curve", solidHeader = T,
        width = 8, collapsible = T,
        shinycssloaders::withSpinner(plotlyOutput("line_plot")))
    ),  

    fluidRow(
      tabBox(
        width=12,
      title = "",
      selected = "Global Statistics",
      tabPanel("Global Statistics", 
        shinycssloaders::withSpinner(
        plotOutput("Global_plot")
        )),
      tabPanel("Vaccine Statisics", 
            box(
            width = 12,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("vaccine_table")), 
            style = "font-size: 70%;")),
      tabPanel("Therapeutics Statisics", 
            box(
            width = 12,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("therapeutics_table")), 
            style = "font-size: 70%;")),
      tabPanel("Medical Summary Table", 
            box(title = "current therapeutics candidates ",
            width = 6,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("Summary_table1")), 
            style = "font-size: 70%;"),
            box(title = "current vaccine candidates ",
            width = 6,
            collapsible = T,
            shinycssloaders::withSpinner(DT::dataTableOutput("Summary_table2")), 
            style = "font-size: 70%;")
      ),
      tabPanel("Active per Million", plotlyOutput("active_plot")),
      tabPanel("Deaths per Million", plotlyOutput("Mortality_plot")),
      tabPanel("Cor Plot", plotlyOutput("cor_plot")),
      tabPanel("Daily Increase Curve", plotlyOutput("wave_plot"))  
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
    country_list <- filter(lastest_data$table, updated == lastest_data$time) %>% 
                            arrange(desc(cases)) %>% .$country
    updateSelectizeInput(session, 'country', choices = country_list, server = TRUE)
    
    t = historical_data$time
    
    # update province list
    observe({
        province_list <- unique(subset(historical_data$province, country == input$country)$province)
        updateSelectInput(session, "province", choices = c("All",province_list))
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
    # prepare the table content
    df <- reactive({
        if ( input$province == "All" ) {
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
    # mutate(diff = cases - dplyr::lag(cases, default =  dplyr::first(cases))) -> a
    mutate(diff = cases - lag(cases, default =  first(cases))) -> a
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
                    t, icon = icon("hospital"), color = "green")
    })

    output$summary_dead <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$date == t),]$deaths, " Deaths"), 
                    t, icon = icon("skull-crossbones"), color = "red")
})

# Growth Curve
    output$line_plot <- renderPlotly({
        validate(need(input$country != "", "Loading"))
        x = gather(df(), curve, count, -date)
        p = ggplot(x, aes(date, count, color = curve)) +
            geom_point() + geom_line() + xlab(NULL) + ylab(NULL) +
            scale_color_manual(values=c("#f39c12", "#dd4b39", "#00a65a")) +
            theme_bw() + 
            theme(legend.position = "none") +
                theme(axis.text = element_text(angle = 15, hjust = 1)) +
                scale_x_date(date_labels = "%Y-%m-%d")
        ggplotly(p)
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


    output$Global_plot <- renderPlot({
        validate(need(input$type != "", "Loading"))
        plot(lastest_data,type=input$type, title="")

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
        ggplot(df, aes(country,activePerOneMillion)) + 
            geom_col(color="firebrick")  + scale_x_discrete(limits= df$country) +
            geom_hline(yintercept = mean(df$activePerOneMillion)) + 
            theme_minimal() +
            theme(  axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    legend.position = 'none')
    })

    output$Mortality_plot <- renderPlotly({
        validate(need( exists("lastest_data"), "Loading"))
        df = lastest_data$detail
        df = df[order(df$deathsPerOneMillion,decreasing = T),] 
        ggplot(df, aes(country,deathsPerOneMillion)) + 
            geom_col(color="firebrick")  + scale_x_discrete(limits= df$country) +
            geom_hline(yintercept = mean(df$deathsPerOneMillion)) + 
            theme_minimal() +
            theme(  axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    legend.position = 'none')
    })

    # cor_plot
    output$cor_plot <- renderPlotly({
        validate(need( exists("lastest_data"), "Loading"))
        df = lastest_data$detail 
        type1 = input$cor_type1
        type2 = input$cor_type2
        p = ggplot(df, aes_string(type1,type2,size=type2, text="country")) + 
            geom_jitter(color="firebrick") +  
            theme_minimal() + 
            labs(subtitle="The size of the dots in the graph corresponds to the number of patients diagnosed today") +
            theme(  axis.text=element_blank(),
                    axis.ticks=element_blank(),
                    legend.position = 'none' )  
        
        ggplotly(p, tooltip = c("country","x","y"))
})
    # wave_plot
    output$wave_plot <- renderPlotly({
        validate(need(input$country_list2, "Please Choose At lease one Country from the left side panel"))
        tmp = subset(a, country %in% input$country_list2)
        p <- ggplot(tmp,aes(date,log(diff+1),color=country)) + geom_line() + 
                    labs(y="Log2 of daily increase cases") + 
                    theme(axis.text = element_text(angle = 15, hjust = 1)) +
                    scale_x_date(date_labels = "%Y-%m-%d") + theme_minimal()
            ggplotly(p)
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