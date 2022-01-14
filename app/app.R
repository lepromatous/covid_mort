setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covid_mort/app")

source("both.R")
source("both2.R")
#source("both3.R")
source("both4.R")
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(RCurl)

    ui<-dashboardPage( skin = "purple",
        dashboardHeader(title = "COVID-19 Mortality", titleWidth = 320,
                        tags$li(a(target = "_blank", href = 'https://www.slu.edu/', img(src = 'slu.jpg',
                                title = "Saint Louis University", height = "40px"),
                                style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown"),
                        tags$li(a(target = "_blank", href = 'https://www.slu.edu/research/research-institute/big-ideas/ahead/index.php', img(src = 'ahead.jpg',
                                title = "AHEAD Institute", height = "40px"),
                                style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown"),
                        tags$li(a(target = "_blank", href = 'https://www.slu.edu/research/research-institute/big-ideas/ahead/index.php', img(src = 'sipc.png',
                                title = "Systems Infection Prevention Center", height = "40px"),
                                style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown")),
    
        dashboardSidebar(width = 320,
            sidebarMenu(
                br(),
                menuItem("COVID-19 Mortality by Country", tabName = "countrymort"),
                menuItem("COVID-19 Mortality in the US by State and County", tabName = "usmortality"),
                menuItem("COVID-19 in Missouri", tabName = "cplink"),
                #menuItem("Saint Louis COVID-19 Task Force", tabName = "stltf"), 
                menuItem("Analyze your own data", tabName = "template"),
                hr(),
                p("All figures plot"), 
                p(HTML(paste(HTML('&emsp;'), "exponentially weighted moving averages"))),
                p(HTML(paste(HTML('&emsp;'), HTML('&emsp;'), "of 56 days (4 incubation periods)"))),
                hr(),
                p(a("Learn more about anomaly detection.", target="_blank", href="https://business-science.github.io/anomalize/")),
                HTML(paste(HTML('&emsp;'), "We used STL and GESD for Anomaly Detection")),
                p(),
                p(a("Learn more about breakout detection.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2014/breakout-detection-in-the-wild.html")),
                HTML(paste(HTML('&emsp;'), "We used Twitter's Breakout Detection method")),
                hr(),
                HTML(paste(strong("Created by:"), "Timothy Wiemken, PhD", "Samson Niemotka, BS", "Christopher Prener, PhD", sep="<br/>")),
                p(a("Email Us", target="_blank", href="mailto:timothy.wiemken@health.slu.edu")), 
                hr(),
                "Version 1.4, Jan 14, 2022"
            )),
        dashboardBody(
                tags$head(tags$style(HTML('
                  .main-header .logo {
                    font-family: "Georgia", Times, "Times New Roman", serif;
                    font-weight: bold;
                    font-size: 24px;
                  }
                '))),
            tabItems(
                tabItem(tabName = "countrymort",
                        column(6, selectInput('countrydropdown', "Choose your country of interest", choices=sort(countrylist), selected="US")),
                        column(6, sliderInput('breakslider_1', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                        plotOutput("plot_both"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "Data Source (Mortality by Country), Center for Systems Science and Engineering at Johns Hopkins University, GitHub"), style = "font-size:10px"),
                ),
                
                tabItem(tabName = "usmortality",
                        column(4, selectInput('state', 'Choose your state of interest', choices = statelist)),
                        column(4, selectInput('county', 'Choose your county of interest', "")),
                        column(4, sliderInput('breakslider_2', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                        plotOutput("plot_us"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(tags$a(href="https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv", "Data Source (US Mortality by State/County), New York Times, GitHub"), style = "font-size:10px")
                ),
                
                tabItem(tabName = "cplink",
                        br(),
                        box(width = 12, 
                            actionButton("gotocp", "For more in-depth information about COVID-19 in Missouri, including St. Louis, please click here", icon = icon("info-circle"), onclick ="window.open('https://slu-opengis.github.io/covid_daily_viz/index.html', '_blank')")
                        )
                        
                ),
                # 
                # tabItem(tabName = "stltf",
                #     fluidRow(
                #         column(6, selectInput("newdata", "Select your metric here.", choices = ddlist)),
                #         column(6, sliderInput('breakslider_3', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                #         plotOutput("task"))),
                tabItem(tabName = "template",
                    fluidRow(
                        box(title = "Data template", collapsible = T,
                        downloadButton("template", strong("STEP 1: Download the template")),
                        br(),
                        br(),
                        fileInput('file1', strong("STEP 2: Upload your data from the template (.csv format)"),
                            accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                        htmlOutput("compat")
                        ),
                        box(title = "Plot options", collapsible = T,
                            column(4, textInput("amount", "X-axis ticks to divide by:", value = 1)),
                            column(4, selectInput("breaker", "Show X-axis ticks by:", choices = c("day", "week", "month", "year"), selected = "month")),
                            column(4, textInput("xaxis", "X-axis label")),
                            column(12, textInput("yaxis", "Y-axis label")),
                            column(12, sliderInput('breakslider_4', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200))
                            ),
                        conditionalPanel(condition = "output.compat != 'Error: Data needs at least 15 rows for analysis.' & output.compat != 'Error: Uploaded data has too many columns. Please use the template with two columns, date and count or rate.'",
                        plotOutput("custom"))
                        ))
        ) #TabItems
    ) #DashboardBody 
) #DashboardPage
    
    
    
    
    
    
    
    
#     
#     
#     
#     
#     
#     br(),
#             br(),
#             #sliderInput('alphaslider', "Width of the normal range:",min=0.01, max=0.5, value=0.05, step=0.01, width=200),
#             #sliderInput('anomslider', "Maximum proportion of anomalies:",min=0.05, max=1, value=.2, step=.05, width=200),
#             #sliderInput('breakslider', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200),
#             br(),
#             #p("Want to learn more about anomaly detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series.html"),
#             br(),
#             br(),
#             #p("Want to learn more about breakout detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2014/breakout-detection-in-the-wild.html"),
#             br(), 

#             p(tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "Data Source (Mortality by Country), Center for Systems Science and Engineering at Johns Hopkins University, GitHub"), style = "font-size:10px"),
#             p(tags$a(href="https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv", "Data Source (US Mortality by State/County), New York Times, GitHub"), style = "font-size:10px")
#             



library(shinydashboard)

server <- function(input, output, session) {
    set.seed(122)
    
    outVar <- reactive({
        mydata <- input$state
        mydata
    })
    
    observe({
        updateSelectInput(session, "county", choices = c("All", sort(as.character(unique(data$county[data$state == outVar()[1]]))))
        )})
    
    output$plot_both <- renderPlot({
        anombreak.covid.mort(alpha = 0.05, max_anoms = 0.2, country = input$countrydropdown, n.break=input$breakslider_1)
    })
    
    output$plot_us <- renderPlot({
        anombreak.covid.mort_sc(alpha = 0.05, max_anoms = 0.2, n.break=input$breakslider_2, state = input$state, county = input$county)
    })
    
    # output$task <- renderPlot({
    #     anombreak.covid.mort_task(alpha=0.05, subchoice = input$newdata, max_anoms=0.2, n.break=input$breakslider_3)
    # })
    output$template <- downloadHandler(
        filename="template.csv",  # desired file name on client 
        content=function(file){
            write.csv(read.csv("template.csv", stringsAsFactors = F), file, row.names=F, na="")
    })
    output$custom <- renderPlot({
        inFile<-input$file1
        if(is.null(inFile)) return(NULL)
        dataup<-read.csv(inFile$datapath, sep = ",", header = T, fill = T)
        anombreak.covid.mort_upload(dataup, alpha=0.05, max_anoms=0.2, n.break=input$breakslider_4, yaxis=input$yaxis, xaxis = input$xaxis, breaks = paste(input$amount, input$breaker), date.labz = ifelse(input$breaker%in%c("year", "month"), "%Y %b", "%Y-%b-%d"))
    })
    output$compat<-renderText({
        inFile<-input$file1
        if(is.null(inFile)) return(NULL)
        dataup<-read.csv(inFile$datapath, sep = ",", header = T, fill = T)
        compat(dataup)
    })

}
shinyApp(ui = ui, server = server)