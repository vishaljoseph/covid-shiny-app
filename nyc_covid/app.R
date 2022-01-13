library(shiny)
library(tidyverse)
library(vroom)
library(leaflet)
library(htmlwidgets)
library(plotly)

setwd("~/Desktop/CPR/sample/nyc_covid")

#read in saved rds files
all_geo <- readRDS("all_geo.RDS")
all_bor <- readRDS("all_bor.RDS")
trend_df <- readRDS("trend_df.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC COVID-19 Data Visualization"),
    h4("The data was obtained from the NYC Health github repository and all data
       in this repo is sourced from the NYC Department of Health. The variables chosen
       for the below visualisations are case rate, testing rate and percent positives.
       Case rate and testing rate are the value at the end of a given week per
       100,000 people. Percent positive indicates the percentage of people that
       tested for COVID-19 with a molecular test who tested positive.
       
       The map below shows the variation for each of these variables over time across
       zip codes. The bar plot to the right of the map depicts the variation of
       these variables across the 5 boroughs of New York - Bronx(BX), Brooklyn(BN),
       Manhattan(MN), Queens(QN) and Staten Island(SI)."),
    br(),
    wellPanel(
        fluidRow(
            column(
                6,
                radioButtons("metric", "Metric",
                             c("Case rate" = "case_rate",
                               "Test rate" = "test_rate",
                               "Percent Positive" = "pc_positive"),
                             inline = T)
            )
        ),
        fluidRow(
            column(
                12,
                sliderInput("week",
                            "Week ending",
                            min = min(all_geo$week_ending),
                            max = max(all_geo$week_ending),
                            value = min(all_geo$week_ending),
                            step = 7,
                            animate = T
                            )
            )
        )
    ),
    br(),
    fluidRow(
        column(
            6,
            leafletOutput("geoPlot")
        ),
        column(
            6,
            plotlyOutput("borPlot")
        )
    ),
    br(),
    br(),
    br(),
    h3("Trend plot of mean metric values"),
    h5("To generate the plot below, data was aggregated at the week-ending level
    and the mean was calculated for each of the variables in consideration."),
    br(),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("trend_dates",
                           "Date range",
                           start = min(all_geo$week_ending),
                           end = max(all_geo$week_ending)),
            selectInput("type",
                        label = "Metric",
                        choices = c("Case rate" = "case_rate",
                                    "Test rate" = "test_rate",
                                    "Percent Positive" = "pc_positive"),
                        selected = "case_rate",
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("trendPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    week <- reactive({
        all_geo %>% 
            filter(week_ending == input$week) %>% 
            mutate(metric = get(input$metric)) %>% 
            select(MODZCTA, label, week_ending, geometry, metric)
    })
    
    week_bor <- reactive({
        all_bor %>% 
            filter(week_ending == input$week) %>% 
            mutate(metric = get(input$metric)) %>% 
            select(week_ending, bor, metric)
        # all_bor$bor <- factor(all_bor$bor, levels = unique(all_bor$bor)
        #                       [order(all_bor$metric, decreasing = FALSE)])
    })
    
    week_trend <- reactive({
        if (length(input$type) == 1) {
            trend_df %>%
                filter(week_ending >= input$trend_dates[1], week_ending <= input$trend_dates[2]) %>%
                select(week_ending, !!input$type[1]) %>%
                pivot_longer(2, names_to = "metric_type", values_to = "value")
        } else if (length(input$type) == 2){
            trend_df %>%
                filter(week_ending >= input$trend_dates[1], week_ending <= input$trend_dates[2]) %>%
                select(week_ending, !!input$type[1], !!input$type[2]) %>%
                pivot_longer(2:3, names_to = "metric_type", values_to = "value")
        } else {
            trend_df %>%
                filter(week_ending >= input$trend_dates[1], week_ending <= input$trend_dates[2]) %>%
                select(week_ending, !!input$type[1], !!input$type[2], !!input$type[3]) %>%
                pivot_longer(2:4, names_to = "metric_type", values_to = "value")
        }
    })
    
    # week_trend <- reactive({
    #     if (length(input$type) == 1) {
    #         trend_df %>%
    #             filter(week_ending >= input$trend_dates[1], week_ending <= input$trend_dates[2]) %>%
    #             select(week_ending, !!input$type[1])
    #     } else if (length(input$type) == 2){
    #         trend_df %>%
    #             filter(week_ending >= input$trend_dates[1], week_ending <= input$trend_dates[2]) %>%
    #             select(week_ending, !!input$type[1], !!input$type[2])
    #     } else {
    #         trend_df %>%
    #             filter(week_ending >= input$trend_dates[1], week_ending <= input$trend_dates[2]) %>%
    #             select(week_ending, !!input$type[1], !!input$type[2], !!input$type[3])
    #     }
    # })
    
    output$borPlot <- renderPlotly({
        plot_ly(week_bor(), x = ~bor, y = ~metric, type = 'bar',
                color = ~bor) %>% 
            layout(title = 'Borough-wise variation', showlegend = FALSE) %>% 
            layout(yaxis = list(title = "")) %>%
            layout(xaxis = list(title = ""))
    })
    
    output$geoPlot <- renderLeaflet({
        if (input$metric == "case_rate") {
            pal <- colorBin(palette = "OrRd", 9, domain = all_geo$case_rate)
            labels <- sprintf(
                "<strong>%s</strong><br/>%g cases per 100,000 people",
                week()$MODZCTA, week()$metric) %>%
                lapply(htmltools::HTML)
        } else if (input$metric == "test_rate") {
            pal <- colorBin(palette = "PuBu", 9, domain = all_geo$test_rate)
            labels <- sprintf(
                "<strong>%s</strong><br/>%g tests per 100,000 people",
                week()$MODZCTA, week()$metric) %>%
                lapply(htmltools::HTML)
        } else {
            pal <- colorBin(palette = "OrRd", 9, domain = all_geo$pc_positive)
            labels <- sprintf(
                "<strong>%s</strong><br/>%g percent positive tests per 100,000 people",
                week()$MODZCTA, week()$metric) %>%
                lapply(htmltools::HTML)
        }
        
        week() %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet() %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(lat = 40.700, lng = -73.9442, zoom = 10) %>% 
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~ pal(metric),
                        highlightOptions = highlightOptions(weight = 5,
                                                            fillOpacity = 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>% 
            addLegend("bottomright",
                      pal = pal,
                      values = ~ metric,
                      title = input$metric,
                      opacity = 0.7)
    })
    
    output$trendPlot <- renderPlotly({
        print(week_trend())
        plot_ly(week_trend(), x = ~week_ending, y = ~value, type = 'scatter',
                color = ~metric_type) %>% 
            layout(yaxis = list(title = "")) %>%
            layout(xaxis = list(title = ""))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
